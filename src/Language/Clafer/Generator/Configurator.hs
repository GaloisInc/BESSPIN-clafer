{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, DeriveGeneric, DeriveAnyClass #-}

module Language.Clafer.Generator.Configurator where

import System.Process
import Text.Printf
import qualified Data.StringMap as SM
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import System.FilePath

import Language.Clafer.ClaferArgs
import Language.ClaferT
import Language.Clafer.Intermediate.Intclafer as Inter
import Language.Clafer.Front.AbsClafer
import Language.Clafer.Common
import Language.Clafer.Intermediate.Tracing

type CElementKeyBit = String

type CElementKey = [CElementKeyBit]

appendElementKeyBit :: CElementKey -> CElementKeyBit -> CElementKey
appendElementKeyBit key bit = key ++ [bit]

data CElement = CElement {
    cIdent :: String,
    cParents :: [String],
    cOldUid :: UID,
    cParentKey :: CElementKey,
    cElements :: [CElement]
} deriving(Eq, Show)

extractConcreteTree :: Monad m => SM.StringMap IClafer -> [UID] -> IElement -> m [CElement]
extractConcreteTree uidMap parents (IEClafer claf) = do
    superElements <- maybe (return []) (inlineSuperElements claferUid) $ maybeSuper
    -- liftIO . putStrLn $ id ++ " --> " ++ (show maybeSuper)
    childElements <- concatMapM (extractConcreteTree uidMap (parents ++ [claferUid])) $ _elements claf
    let allChildElements = superElements ++ childElements
    return [CElement { cIdent = clafId, cParents = parents, cParentKey = parentIdents, cOldUid = claferUid, cElements = allChildElements }]
    where
        clafId = _ident claf
        claferUid = _uid claf
        parentIdents = map (\x -> _ident $ uidMap SM.! x) parents
        maybeSuper = listToMaybe $ getSuper claf
        concatMapM f tr = liftM concat $ mapM f tr
        inlineSuperElements currentUid parentUid = do
            let iclaf = uidMap SM.! parentUid
            -- liftIO . putStrLn $ show iclaf
            let elems = _elements iclaf
            -- liftIO . putStrLn $ show $ length elements
            concatMapM (extractConcreteTree uidMap (parents ++ [currentUid])) elems
extractConcreteTree _ _ _ = return []

generateCElementMap :: CElement -> M.Map CElementKey CElement
generateCElementMap cel = generateCElementMap' M.empty cel
    where
        generateCElementMap' currentMap currentElem = 
            let fullKey = cParentKey currentElem `appendElementKeyBit` (cIdent currentElem) in
            let map' = M.insert fullKey currentElem currentMap in
            foldl generateCElementMap' map' $ cElements currentElem

printCElement :: CElement -> String
printCElement = printCElement' ""
    where
        printCElement' spaces el =
            let line = printf "%s I: %s, P: (%s), ID: [%s], OID: %s" spaces (cIdent el) (intercalate ", " $ cParents el) (fullId el) (cOldUid el) in
            let rest = intercalate "\n" $ map (printCElement' $ spaces ++ "  ") $ cElements el in
            line ++ (if length rest > 0 then "\n" else "") ++ rest
        parentIds el = (intercalate "." $ cParentKey el)
        fullId el = if (length (parentIds el) > 0) then parentIds el ++ "." ++ cIdent el else cIdent el

printList :: (MonadIO m, Foldable t, Show a) => t a -> m ()
printList lst = liftIO $ mapM_ (putStrLn . show) lst

generateElementExpressions :: M.Map CElementKey a -> M.Map CElementKey Exp
generateElementExpressions elemMap = M.mapWithKey (\idlist _ -> makeExpressionFromNames idlist) elemMap
    where
        makeExpressionFromNames idList = foldl1 (EJoin noSpan) $ map exprFromName idList
        exprFromName nm = ClaferId noSpan $ Path noSpan $ [ModIdIdent noSpan $ makePosIdent nm]
        makePosIdent nm = PosIdent ((0::Int, -1::Int), nm)

generateAssertions :: M.Map CElementKey a -> M.Map CElementKey (Assertion, Assertion)
generateAssertions elemMap =
    let elemExprMap = generateElementExpressions elemMap in
    evalState (mapM makeAssertionPair elemExprMap) (1000 :: Integer)
    where
        makeAssertionPair :: Exp -> State Integer (Assertion, Assertion)
        makeAssertionPair expr = do
            i <- get
            let posAssert = Assertion noSpan [adjustExprWithUniqueLine expr i]
            let negExpr =  EQuantExp noSpan (QuantNot noSpan) expr
            let negAssert = Assertion noSpan [adjustExprWithUniqueLine negExpr $ i + 1]
            put $ i + 2
            return (posAssert, negAssert)
        adjustExprWithUniqueLine e@(ClaferId _ nm) i =
            let l = length $ show e in
            ClaferId (fixSpan i l) nm
        adjustExprWithUniqueLine e@(EJoin _ e1 e2) i = 
            let l = length $ show e in
            EJoin (fixSpan i l) e1 e2
        adjustExprWithUniqueLine e@(EQuantExp _ q expr) i =
            let l = length $ show e in
            EQuantExp (fixSpan i l) q expr
        adjustExprWithUniqueLine e _ = e
        fixSpan i l =
            let (i', l') = (toInteger i, toInteger l) in
            Span (Pos i' 0) (Pos i' l') 

extractKeyFromExp :: Exp -> Maybe CElementKey
extractKeyFromExp e =
    case e of
        EJoin _ e1 e2 -> (++) <$> extractKeyFromExp e1 <*> extractKeyFromExp e2
        ClaferId _ _ -> exp2Name e
        _ -> Nothing
    where
        exp2Name (ClaferId _ (Path _ [ModIdIdent _ (PosIdent (_, nm))])) = Just [nm]
        exp2Name _ = Nothing

extractPositiveConstraint :: Constraint -> [CElementKey]
extractPositiveConstraint (Constraint _ exprs) = catMaybes $ map extractKeyFromExp exprs

extractNegativeConstraint :: Constraint -> [CElementKey]
extractNegativeConstraint (Constraint _ exprs) = catMaybes $ map extractNegExp exprs
    where
        extractNegExp e =
            case e of
                ENeg _ expr -> extractKeyFromExp expr
                _ -> Nothing

generateAssertionsToCheck :: Integral k =>
                               M.Map CElementKey a
                               -> Module
                               -> [CElementKey]
                               -> [CElementKey]
                               -> (Module, M.Map k CElementKey)
generateAssertionsToCheck elemMap ast setFeatures unsetFeatures =
    let elemsToProcess = M.withoutKeys elemMap $ S.fromList (setFeatures ++ unsetFeatures)
    -- liftIO $ printf "%d --> %d\n" (M.size elemMap) (M.size elemsToProcess)
        assertionsMap = generateAssertions elemsToProcess
        assertionPairsList = M.toList assertionsMap
        indexedAssertionMap = M.fromList $ zipWith (\i (key, _) -> (i, key)) [0..] assertionPairsList
        allAssertions = concatMap (\(_, (a1, a2)) -> [a1, a2]) assertionPairsList
        updatedAst = updateAstWithAssertions allAssertions in
    (updatedAst, indexedAssertionMap)
    where
        updateAstWithAssertions assertions =
            let assertionDecls = map (ElementDecl noSpan . SubAssertion noSpan) assertions in
            let (Module sp declarations) = ast in
            Module sp $ declarations ++ assertionDecls

filterConcreteTree :: (CElement -> Bool) -> CElement -> CElement
filterConcreteTree f tree = head $ filterConcreteTree' [tree]
    where
        mapTreeWithFilteredChildren tree' = do
            let children = filterConcreteTree' $ cElements tree'
            tree' { cElements = children }
        filterConcreteTree' trees = do
            let trees' = filter f trees
            map mapTreeWithFilteredChildren trees'

runAlloyCheck :: (MonadIO m, Integral k) => String -> String -> m (Bool, M.Map k (Bool, Bool))
runAlloyCheck toolPath filename = do
    let classPath = toolPath ++ "alloy4.2.jar" ++ [searchPathSeparator] ++ toolPath ++ "alloy-check\\"
    (_, out, _) <- liftIO $ readProcessWithExitCode "java" ["-cp", classPath, "AlloyCheck", filename] ""
    -- TODO: Error check
    let allBlocks = blocks $ lines out
    traceShow allBlocks $
     if checkValid (head allBlocks)
        then return (True, M.fromList $ getIndexedAssertionPairs $ tail allBlocks)
        else return (False, M.empty)
    where
        blocks ls = pickBlocks $ tail ls
        pickBlocks ls =
            let (block, rest) = break blockP  $ tail ls in
            if length rest > 0 then block:(pickBlocks rest) else [block]
        blockP = ("===" `isPrefixOf`)
        checkValid block = "---INSTANCE---" `isPrefixOf` (head block)
        checkAssertion block = "---OUTCOME---" `isPrefixOf` (head block)
        getAssertionPairResults [] = []
        getAssertionPairResults (b1:b2:t) = (checkAssertion b1, checkAssertion b2) : getAssertionPairResults t
        getAssertionPairResults _ = undefined
        getIndexedAssertionPairs bs = zip [0..] $ getAssertionPairResults bs


analyzeAstForFeatures :: Module -> ([CElementKey], [CElementKey])
analyzeAstForFeatures ast =
    let setFeatures = concatMap extractPositiveConstraint currentConstraints
        unsetFeatures = concatMap extractNegativeConstraint currentConstraints 
    in
    (setFeatures, unsetFeatures)
    where
        justConstraint (Subconstraint _ c) = Just c
        justConstraint _ = Nothing
        currentConstraints =
            let (Module _ declarations) = ast in
            let elems = map (\(ElementDecl _ e) -> e) declarations in
            catMaybes $ map justConstraint elems

generateFinalResults :: [CElementKey] -> [CElementKey] -> M.Map CElementKey (Bool, Bool) -> M.Map String String
generateFinalResults setFeatures unsetFeatures assertionResultsMap =
    let setCollection = map (\x -> (stringUid x, "USERSELECTED")) setFeatures
        unsetCollection = map (\x -> (stringUid x, "USERREJECTED")) unsetFeatures
        mapped = M.mapKeys stringUid $ M.map determineState assertionResultsMap 
    in
    mapped `M.union` M.fromList setCollection `M.union` M.fromList unsetCollection
    where
        stringUid key = intercalate "." key
        determineState (True, False) = "FORCEDON"
        determineState (False, True) = "FORCEDOFF"
        determineState (_, _) = "UNCONFIGURED"

generateConfigurationStatus :: MonadIO m => IModule -> GEnv -> (Module -> [ClaferMode] -> ClaferT m (M.Map ClaferMode String)) -> ClaferT m (Either String (M.Map String String))
generateConfigurationStatus im ge generateOutputs = do
    -- Construct a map of keys to concrete, configurable nodes. Note that we exclude nodes that
    -- are not really clafers (for example, nodes that have integer values)
    let concretes = filter isConcrete $ _mDecls im
    let uidMap = uidClaferMap ge
    fullElems <- concatMapM (extractConcreteTree uidMap []) concretes
    let elems = map (filterConcreteTree $ notPrimitive ge) fullElems 
    let cElemMap = foldl M.union M.empty $ map generateCElementMap elems  
    -- Extract features that have been explicitly set or unset  
    ast <- getAst
    let (setFeatures, unsetFeatures) = analyzeAstForFeatures ast
    -- And then generate assertions corresponding to the rest
    let (updatedAst, indexedAssertionMap) = generateAssertionsToCheck cElemMap ast setFeatures unsetFeatures
    -- Use alloy to check the assertions and process to get a mapping of features to their current status
    resultsMap <- generateOutputs updatedAst [Alloy]
    let alloyCode = resultsMap M.! Alloy
    liftIO $ writeFile "test_me.alloy" alloyCode 
    (isvalid, indexedAssertionResultMap) <- runAlloyCheck ".\\" "test_me.alloy"
    let finalRes = if isvalid
                        then do
                            let res = generateAssertionResults indexedAssertionMap indexedAssertionResultMap
                            let res' = generateFinalResults setFeatures unsetFeatures res
                            Right res'
                        else Left "The configuration is invalid"
    return finalRes
    where
        concatMapM f tr = liftM concat $ mapM f tr
        isConcrete (IEClafer claf) = not $ _isAbstract claf
        isConcrete _ = False
        notPrimitive ge cel = do
            let iclaf = (uidClaferMap ge) SM.! (cOldUid cel)
            all (not . isPrimitive) $ getReference iclaf
        generateAssertionResults :: M.Map Int [String] -> M.Map Int (Bool, Bool) -> M.Map [String] (Bool, Bool)
        generateAssertionResults indexedAssertionMap indexedAssertionResultMap = do
            M.foldlWithKey (\res i v -> M.insert v (indexedAssertionResultMap M.! i) res) M.empty indexedAssertionMap
        -- writeJSON results file = liftIO $ writeFile file $ convertString $ encode results
