{-# LANGUAGE NamedFieldPuns #-}

module Language.Clafer.Generator.Choco (genCModule) where

import Language.Clafer.Intermediate.ScopeAnalyzer
import Control.Monad
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Prelude hiding (exp)
import System.Process
import Language.Clafer.ClaferArgs
import Language.Clafer.Common
import Language.Clafer.Front.Absclafer
import Language.Clafer.Intermediate.Intclafer
import Debug.Trace


genCModule :: ClaferArgs -> (IModule, GEnv) -> Result
genCModule _ (imodule@IModule{mDecls}, _) =
    (genConcreteClafer =<< concreteClafers)
    ++ (genAbstractClafer =<< abstractClafers)
--    ++ (genConstraint =<< concreteClafers)
    ++ (genGroupCardinality =<< clafers)
    ++ (genRefClafer =<< clafers)
    -- "Prints the solution.
    ++ "function solution() {\n"
    ++ "    solution__root(0, \"\");\n"
    ++ "}\n"
    ++ "\n"
    ++ (genToString =<< concreteClafers)
    where
    root :: IClafer
    root = IClafer noSpan False Nothing "root" "root" (ISuper False [PExp Nothing "" noSpan $ IClaferId "clafer" "clafer" True]) (Just (1, 1)) (0, 0) mDecls
    
    toplevelClafers = mapMaybe iclafer mDecls
    abstractClafers = filter isAbstract toplevelClafers
    parentChildMap = childClafers root
    clafers = root : map snd parentChildMap
    concreteClafers = filter (not . isAbstract) clafers
    minusRoot = filter ((/= "root") . uid)
    
    claferWithUid u = fromMaybe (error $ "claferWithUid: \"" ++ u ++ "\" is not a clafer") $ find ((== u) . uid) clafers
    
    prims = ["int", "integer", "string", "real"]
    
    -- All abstract clafers u inherits
    supersOf u =
        case super $ claferWithUid u of
            ISuper False [PExp{exp = IClaferId{sident}}]
                | sident == "clafer"  -> []
                | sident `elem` prims -> []
                | otherwise           -> sident : supersOf sident
            _ -> []
            
    refOf u =
        case super $ claferWithUid u of
            ISuper True [PExp{exp = IClaferId{sident}}] -> Just sident
            ISuper False [PExp{exp = IClaferId{sident}}]
                | sident == "int"     -> Just "integer"
                | sident `elem` prims -> Just sident
                | otherwise           -> Nothing
            _ -> Nothing
            
    -- All concrete clafers that inherit u
    subsOf u = [uid | IClafer{uid} <- concreteClafers, u `elem` supersOf uid]
    subClafersOf = map claferWithUid . subsOf

    parentOf u = fst $ fromMaybe (error $ "parentOf: \"" ++ u ++ "\" is not a clafer") $ find ((== u) . uid . snd) parentChildMap
    parentClaferOf = claferWithUid . parentOf
    -- Direct childrens
    childrenOf = map uid . childrenClaferOf
    childrenClaferOf u = [c | (p, c) <- parentChildMap, p == u]
    
    -- Indirect childrens
    indirectChildrenOf u = childrenOf =<< supersOf u
    indirectChildrenClaferOf u = childrenClaferOf =<< supersOf u
    
    genConcreteClafer :: IClafer -> Result
    genConcreteClafer IClafer{uid} =
        do
            IClafer{isAbstract, uid = cuid, card}  <- childrenClaferOf uid
            guard $ not isAbstract
            let scope = scopeOf uid
            let cScope = scopeOf cuid
            let name = "__" ++ cuid
            cuid ++ " = set(\"" ++ cuid ++ "\", 0, " ++ show (cScope - 1) ++ ");\n"
                ++ genClafer' uid scope name cScope card
                ++ "addConstraint(setUnion(" ++ name ++ ", " ++ cuid ++ "));\n"
                ++ cuid ++ "__parent = intArray(\"" ++ cuid ++ "__parent\", " ++ show (cScope) ++ ", 0, " ++ show scope ++ ");\n"
                ++ "addConstraint(inverseSet(" ++ cuid ++ "__parent, " ++ name ++ ".concat(setArray(\"" ++ name ++ "__unused\", 1, 0, " ++ show (cScope - 1) ++ "))));\n"
                --addConstraint(inverseSet(intArray("B_parent", 4, 0, 2), __c2_B.concat(setArray("__c2_B_unused", 1, 0, 3))));
                ++ "\n"
                
    genRefClafer :: IClafer -> Result
    genRefClafer IClafer{uid} =
        fromMaybe "" $ do
            ref <- refOf uid
            let code
                    | ref == "integer" =
                        name ++ " = intArray(\"" ++ name ++ "\", " ++ show scope ++ ");\n"
                    | otherwise =
                        name ++ " = intArray(\"" ++ name ++ "\", " ++ show scope ++ ", 0, " ++ show (scopeOf ref - 1) ++ ");\n"
            let diff = 
                    "for(var diff1 = 0; diff1 < " ++ show scope ++ "; diff1++) {\n"
                    ++ "    var refdiff = [];\n"
                    ++ "    for(var diff2 = diff1 + 1; diff2 < " ++ show scope ++ "; diff2++) {\n"
                    ++ "        refdiff.push(implies(eq(" ++ parent ++ "[diff1], " ++ parent ++ "[diff2]), neq(" ++ name ++ "[diff1], " ++ name ++ "[diff2])));\n"
                    ++ "    }\n"
                    ++ "    addConstraint(implies(neq(" ++ parent ++ "[diff1], " ++ show parentScope ++ "), and(refdiff)));\n"
                    ++ "}\n"
            return $ code ++ diff
        where
        name = uid ++ "__ref"
        parent = uid ++ "__parent"
        scope = scopeOf uid
        parentScope = scopeOf (parentOf uid)
        
    genAbstractClafer :: IClafer -> Result
    genAbstractClafer IClafer{uid} =
        do
            IClafer{isAbstract, uid = cuid, card}  <- childrenClaferOf uid
            let subs = subsOf uid
            let scope = scopeOf uid
            let sScopes = map scopeOf subs
            let cScope = scopeOf cuid
            let names = ["__" ++ cuid ++ "__" ++ sub | sub <- subs]
            cuid ++ " = set(\"" ++ cuid ++ "\", 0, " ++ show (cScope - 1) ++ ");\n"
                ++ concat [genClafer' suid sScope name cScope card | (suid, sScope, name) <- zip3 subs sScopes names]
                ++ cuid ++ "__parent = intArray(\"" ++ cuid ++ "__parent\", " ++ show (cScope) ++ ", 0, " ++ show scope ++ ");\n"
                ++ "addConstraint(inverseSet(" ++ cuid ++ "__parent, " ++ concatArrays (names ++ ["setArray(\"" ++ cuid ++ "__unused\", 1, 0, " ++ show (cScope - 1) ++ ")"]) ++ "));\n"
                ++ "\n"
        where
        concatArrays [x] = x
        concatArrays (x : xs) = x ++ ".concat(" ++ intercalate ", " xs ++ ")"
        
    genClafer' parent parentScope uid scope card =
        uid ++ " = setArray(\"" ++ uid ++ "\", " ++ show parentScope ++ ", 0, " ++ show (scope - 1) ++ ");\n"
        ++ "for (var i = 0; i < " ++ uid ++ ".length; i++) {\n"
        ++
            case card of
                Just (low, -1)   ->
                    "    addConstraint(implies(member(i , " ++ parent ++ "), geqCard(" ++ uid ++ "[i], " ++ show low ++ ")));\n"
                Just (low, high)
                    | low == high ->
                        "    addConstraint(implies(member(i, " ++ parent ++ "), eqCard(" ++ uid ++ "[i], " ++ show low ++ ")));\n"
                    | otherwise   ->
                        "    addConstraint(implies(member(i, " ++ parent ++ "), and([geqCard(" ++ uid ++ "[i], " ++ show low ++ "), leqCard(" ++ uid ++ "[i], " ++ show high ++ ")])));\n"
        ++ "    addConstraint(implies(notMember(i, " ++ parent ++ "), eqCard(" ++ uid ++ "[i], 0)));\n"
        ++ "}\n"

    genGroupCardinality IClafer{gcard = Nothing} = ""
    genGroupCardinality IClafer{uid, gcard = Just (IGCard _ card)}
        | length children > 0 =
            "for (var i = 0; i < " ++ show (scopeOf uid) ++ "; i++) {\n"
            ++ genGroupCardinality' (fst card) (snd card)
            ++ "}\n"
        | otherwise = ""
        where
        genGroupCardinality' 0 (-1) = ""
        genGroupCardinality' 0 high = "    addConstraint(implies(member(i, " ++ uid ++ "), leq(" ++ childrenUnion ++ ", " ++ asNumber high ++ ")));\n"
        genGroupCardinality' low high = "    addConstraint(implies(member(i, " ++ uid ++ "), between(" ++ asNumber low ++ ", " ++ childrenUnion ++ ", " ++ asNumber high ++ ")));\n"

        children = childrenOf uid
        childrenUnion = "sum([" ++ (intercalate ", " $ ["__" ++ child ++ "[i].getCard()" | child <- children]) ++ "])"

    asNumber (-1) = "Number.POSITIVE_INFINITY"
    asNumber num = show num

    genConstraint :: IClafer -> Result
    genConstraint IClafer{elements} =
         unlines $ genConstraint' =<< map exp (mapMaybe iconstraint elements)
        where
    genConstraint' (IFunExp "=" [PExp{exp = IFunExp "#" [arg1]}, arg2]) =
        do
            arg1' <- genConstraint' (exp arg1)
            arg2' <- genConstraint' (exp arg2)
            return $ "addConstraint(eqCard(" ++ arg1' ++ ", " ++ arg2' ++ "));\n";
    genConstraint' (IFunExp "." [PExp{iType = Just (TClafer [thisType]), exp = IClaferId{sident = "this"}}, PExp{exp = IClaferId{sident = child}}]) =
        ["__" ++ child ++ "[" ++ show i ++ "]" | i <- [0 .. scopeOf thisType - 1]]
    genConstraint' e = error $ "genConstraint: " ++ show e
    

    genToString :: IClafer -> Result
    genToString c@IClafer{ident, uid = puid, elements} =
        "function solution__" ++ puid ++ "(parent, indent) {\n"
        ++ "    var i = getVar(" ++ name ++ "[parent]).getValue();\n"
        ++ "    for (var j in i) {\n"
        ++ "        var k = i[j];\n"
        ++ "        println(indent + \"" ++ ident ++ "\" + k);\n"
        ++ concat ["        solution__" ++ cuid ++ "(k, \"    \" + indent);\n" | IClafer{uid = cuid} <- children]
        ++ concat [
            "        __" ++ cuid ++ " = __" ++ cuid ++ "__" ++ puid ++ ";\n"
            ++ "        solution__" ++ cuid ++ "(k, \"    \" + indent);\n"
            | IClafer{uid = cuid} <- indirectChildren]
        ++ (case refOf puid of
                Just "integer" ->
                    "        println(\"    \" + indent + \"ref = \" + getVar(" ++ puid ++ "__ref[k]).getValue());\n"
                Just ref ->
                    "        println(\"    \" + indent + \"ref = " ++ sidentOf ref ++ "\" + getVar(" ++ puid ++ "__ref[k]).getValue());\n"
                Nothing  -> "")
        ++ "    }\n"
        ++ "}\n"
        ++ "\n"
        where
        name = "__" ++ puid
        children = filter isNotAbstract $ childrenClaferOf puid
        indirectChildren = filter isNotAbstract $ indirectChildrenClaferOf puid
                
    sidentOf u = ident $ claferWithUid u
    scopeOf i = fromMaybe 1 $ lookup i scopes
    scopes = scopeAnalysis imodule


isNotAbstract :: IClafer -> Bool
isNotAbstract = not . isAbstract

iclafer :: IElement -> Maybe IClafer
iclafer (IEClafer c) = Just c
iclafer _ = Nothing

iconstraint :: IElement -> Maybe PExp
iconstraint (IEConstraint _ pexp) = Just pexp
iconstraint _ = Nothing

childClafers :: IClafer -> [(String, IClafer)]
childClafers IClafer{uid, elements} =
    childClafers' uid =<< mapMaybe iclafer elements
    where
    childClafers' parent c@IClafer{uid, elements} = (parent, c) : (childClafers' uid  =<< mapMaybe iclafer elements)

implies :: MonadPlus m => Bool -> m a -> m a
implies cond result = if cond then result else mzero
