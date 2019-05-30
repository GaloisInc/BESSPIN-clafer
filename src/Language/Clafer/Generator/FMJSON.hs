{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Language.Clafer.Generator.FMJSON where

import Prelude hiding (exp)
import Control.Lens hiding (elements, children)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Aeson as A
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import Language.Clafer.Common
import Language.Clafer.Intermediate.Intclafer

-- "Unrolled clafer".  Records a single concrete clafer, zero or more abstract
-- clafers that it inherits from, and a list of (unrolled) child clafers.  Each
-- node in the unrolled tree turns into a feature in the FMJSON output.
data UClafer = UClafer
    { _unrolledUid :: UID
    , _concCfr :: IClafer
    , _absCfrs :: [IClafer]
    , _children :: [UClafer]
    }

makeLenses ''UClafer


unsupported :: String -> Either String a
unsupported desc = Left $ "unsupported: " ++ desc

unsupported' :: IClafer -> String -> Either String a
unsupported' cfr desc = unsupported $ desc ++ " (in " ++ cfr ^. uid ++ ")"

unrollClafer :: UIDIClaferMap -> IClafer -> Either String (Maybe UClafer)
unrollClafer uidMap cfr
  | cfr ^. modifiers . abstract = return Nothing
  | otherwise = do
    validateConcrete cfr
    absCfrs' <- gatherSuperClafers uidMap cfr
    mapM_ validateAbstract absCfrs'
    let allElts = concatMap (\c -> c ^. elements) $ cfr : absCfrs'
    children' <- catMaybes <$> mapM (unrollElementClafer uidMap) allElts
    return $ Just $ UClafer "" cfr absCfrs' children'

unrollElementClafer :: UIDIClaferMap -> IElement -> Either String (Maybe UClafer)
unrollElementClafer uidMap (IEClafer cfr) = unrollClafer uidMap cfr
unrollElementClafer _uidMap _ = return Nothing

gatherSuperClafers :: UIDIClaferMap -> IClafer -> Either String [IClafer]
gatherSuperClafers uidMap cfr = case cfr ^. super of
    Just pe -> case pe ^. exp of
        IClaferId { _isTop = isTop', _binding = binding' } -> do
            when (not isTop') $ unsupported' cfr "non-global superclafer"
            superUid <- case binding' of
                GlobalBind u -> return u
                _ -> unsupported' cfr "non-global superclafer"
            superCfr <- case findIClafer uidMap superUid of
                Just x -> return x
                Nothing -> unsupported' cfr $ "superclafer " ++ superUid ++ " not found"
            rest <- gatherSuperClafers uidMap superCfr
            return $ superCfr : rest
        _ -> unsupported' cfr "non-ident superclafer expression"
    Nothing -> return []

validateConcrete :: IClafer -> Either String ()
validateConcrete cfr = do
    when (not $ validGCard $ cfr ^. gcard) $ unsupported' cfr $
        "group cardinality " ++ show (cfr ^. gcard)
    when (not $ validCard $ cfr ^. card) $ unsupported' cfr $
        "cardinality " ++ show (cfr ^. card)
    when (isJust $ cfr ^. reference) $ unsupported' cfr $ "reference clafer"
    when (cfr ^. modifiers ^. initial) $ unsupported' cfr $ "initial clafer"
    when (cfr ^. modifiers ^. final) $ unsupported' cfr $ "initial clafer"

validateAbstract :: IClafer -> Either String ()
validateAbstract cfr = do
    when (not $ validAbstractGCard $ cfr ^. gcard) $ unsupported' cfr $
        "group cardinality " ++ show (cfr ^. gcard) ++ " on abstract clafer"
    when (not $ validAbstractCard $ cfr ^. card) $ unsupported' cfr $
        "cardinality " ++ show (cfr ^. card) ++ " on abstract clafer"
    when (isJust $ cfr ^. reference) $ unsupported' cfr $ "reference clafer"
    when (cfr ^. modifiers ^. initial) $ unsupported' cfr $ "initial clafer"
    when (cfr ^. modifiers ^. final) $ unsupported' cfr $ "initial clafer"


makeUnique :: String -> State (Set String) String
makeUnique s = state $ \seen ->
    let s' = go seen in
    (s', Set.insert s' seen)
  where
    go seen = head $ filter (\s' -> not $ Set.member s' seen) $
        s : [s ++ show i | i <- [0..] :: [Integer]]

assignUidsM :: UClafer -> State (Set String) UClafer
assignUidsM ucfr = do
    name <- makeUnique (ucfr ^. concCfr . ident)
    children' <- mapM assignUidsM $ ucfr ^. children
    return $ ucfr & unrolledUid .~ name & children .~ children'

assignUids :: [UClafer] -> [UClafer]
assignUids ucfrs = evalState (mapM assignUidsM ucfrs) Set.empty


data FeatureCard = FcOff | FcOn | FcOpt
    deriving (Eq, Show)

data GroupCard = GcOpt | GcOr | GcMux | GcXor
    deriving (Eq, Show)

tryConvCard :: Maybe Interval -> Maybe FeatureCard
tryConvCard Nothing = Just FcOn
tryConvCard (Just i) = case i of
    (0, 0) -> Just FcOff
    (0, 1) -> Just FcOpt
    (1, 1) -> Just FcOn
    _ -> Nothing

validCard :: Maybe Interval -> Bool
validCard c = isJust $ tryConvCard c

validAbstractCard :: Maybe Interval -> Bool
validAbstractCard c = c == Just (0, -1)

convCard :: Maybe Interval -> FeatureCard
convCard c = case tryConvCard c of
    Just x -> x
    Nothing -> error $ "invalid feature cardinality: " ++ show c

tryConvGCard :: Maybe IGCard -> Maybe GroupCard
tryConvGCard Nothing = Just GcOpt
tryConvGCard (Just gc) = case gc ^. interval of
    (0, -1) -> Just GcOpt
    (1, -1) -> Just GcOr
    (0, 1) -> Just GcMux
    (1, 1) -> Just GcXor
    _ -> Nothing

validGCard :: Maybe IGCard -> Bool
validGCard gc = isJust $ tryConvGCard gc

validAbstractGCard :: Maybe IGCard -> Bool
validAbstractGCard gc = tryConvGCard gc == Just GcOpt

convGCard :: Maybe IGCard -> GroupCard
convGCard gc = case tryConvGCard gc of
    Just x -> x
    Nothing -> error $ "invalid group cardinality: " ++ show gc


data Feature = Feature
    { _fName :: String
    , _fCard :: FeatureCard
    , _fGCard :: GroupCard
    , _fParent :: Maybe String
    , _fChildren :: [String]
    }

data CExp =
    CeOp String [CExp] |
    CeLit Bool |
    CeFeat String
    deriving (Eq, Show)

makeLenses ''Feature

instance A.ToJSON FeatureCard where
    toJSON fc = (A.toJSON :: Text -> A.Value) $ case fc of
        FcOff -> "off"
        FcOn -> "on"
        FcOpt -> "opt"

instance A.ToJSON GroupCard where
    toJSON gc = (A.toJSON :: Text -> A.Value) $ case gc of
        GcOpt -> "opt"
        GcOr -> "or"
        GcMux -> "mux"
        GcXor -> "xor"

instance A.ToJSON Feature where
    toJSON f = A.object
        [ "name" A..= (f ^. fName)
        , "card" A..= (f ^. fCard)
        , "gcard" A..= (f ^. fGCard)
        , "parent" A..= (f ^. fParent)
        , "children" A..= (f ^. fChildren)
        ]

instance A.ToJSON CExp where
    toJSON (CeOp op' args) = A.object
        [ "kind" A..= ("op" :: Text)
        , "op" A..= op'
        , "args" A..= args
        ]
    toJSON (CeLit val) = A.object
        [ "kind" A..= ("lit" :: Text)
        , "val" A..= val
        ]
    toJSON (CeFeat name) = A.object
        [ "kind" A..= ("feat" :: Text)
        , "name" A..= name
        ]


-- Clafer models map fairly directly to relational models in Alloy.  Each
-- nested IClafer gives rise to a binary relation that relates UClafers derived
-- from its parent IClafer to UClafers derived from itself.  Each non-nested
-- IClafer gives rise to a set (unary relation) of instances of itself.
--
-- FMJSON boolean constraints are not relational.  We need to convert Clafer
-- constraints, which check for non-emptiness of the set resulting from some
-- relational join, into a check of the (boolean) value of a single feature.
--
-- We can do the conversion as long as we can reduce every relational join
-- expression to a set that contains (at most) a single UClafer.  We manage
-- this by requiring each part of the Clafer expression to produce a set with
-- at most one element.
--
--  1. `this` is always one element.
--  2. `x.parent` is always one element.
--  3. Top-level `x` is one element if `x` is instantiated exactly once.  In
--     other words, `x` must appear only once among all `UClafer`s' `concCfr`
--     and `absCfr` fields.
--  4. `x.y` is one element if `x` is one element and `y` is instantiated
--     exactly once within each `x`.  Note this works only because we disallow
--     clafers with cardinality > 1.
--
-- We use `Map UID (Unique String)` for tracking the "instantiated exactly
-- once" property.  When looking up a given IClafer's `uid`, `One name` means
-- there's a unique UClafer instance whose `unrolledUid` is `name`, and `Many`
-- means there may be multiple UClafer instances of this IClafer.

data Unique a = One a | Many
    deriving (Eq, Show)

instance Semigroup (Unique a) where
    _ <> _ = Many

getUnique :: Unique a -> Maybe a
getUnique (One x) = Just x
getUnique _ = Nothing

data Scope = Scope
    -- `unrolledUid` of the parent of this `UClafer`, or `Nothing` if it's at
    -- top  level.
    { _scParent :: Maybe String
    -- Maps UIDs as they appear in `GlobalBind`s to the `unrolledUid` of the
    -- unique instance of that clafer within the current scope.
    , _scResolve :: Map UID (Unique String)
    }

data ConstraintContext = ConstraintContext
    { _ctxCurName :: Maybe String
    , _ctxScopeMap :: Map String Scope
    , _ctxGlobalResolve :: Map UID (Unique String)
    }

makeLenses ''Scope
makeLenses ''ConstraintContext

-- Try to resolve an IExp to a feature name.  Requires the `IExp` to be a set
-- expression producing a set of size either 0 or 1, reflecting whether a
-- single known feature is disabled or enabled.
iExpFeature :: ConstraintContext -> IExp -> Either String String
iExpFeature ctx (IFunExp "." [x, f]) = do
    -- We call `pExpFeature` only at top level, to avoid stacking multiple
    -- "in constraint at ..." clauses onto the error messages.
    parentName <- iExpFeature ctx (x ^. exp)
    sc <- case M.lookup parentName $ ctx ^. ctxScopeMap of
        Just sc -> return sc
        _ -> error $ "no scope for ucfr " ++ show parentName ++ "?"
    childName <- case f ^. exp of
        IClaferId { _sident = "parent" } -> case sc ^. scParent of
            Just name -> return name
            Nothing -> unsupported $ "parent reference from root clafer " ++ show parentName
        IClaferId { _binding = binding' } -> case binding' of
            GlobalBind uid' -> case M.lookup uid' (sc ^. scResolve) of
                Just (One name) -> return name
                Just Many -> unsupported $ "reference to non-unique child " ++ show uid' ++
                    " of " ++ show parentName
                Nothing -> unsupported $ "reference to unknown child " ++ show uid' ++
                    " of " ++ show parentName
            _ -> unsupported $ "non-global binding as RHS " ++ show f ++ " of (.)"
        _ -> unsupported $ "non-ident " ++ show f ++ " as RHS of (.)"
    return childName
iExpFeature ctx (IClaferId { _sident = "this" }) = case ctx ^. ctxCurName of
    Just x -> return x
    Nothing -> unsupported "`this` at top level"
iExpFeature ctx (IClaferId { _sident = sident', _binding = binding' }) = case binding' of
    GlobalBind uid' -> case M.lookup uid' (ctx ^. ctxGlobalResolve) of
        Just (One name) -> return name
        Just Many -> unsupported $ "reference to non-unique global " ++ show uid'
        Nothing -> unsupported $ "reference to unknown global " ++ show uid'
    _ -> unsupported $ "non-global binding for " ++ show sident'
iExpFeature _ctx e = unsupported $ "expression " ++ show e

pExpFeature :: ConstraintContext -> PExp -> Either String String
pExpFeature ctx e = case iExpFeature ctx $ e ^. exp of
    Left err -> Left $ err ++ " (in expression at " ++ show (e ^. inPos) ++ ")"
    Right x -> Right x

iExpConstraint :: ConstraintContext -> IExp -> Either String CExp
iExpConstraint ctx (IDeclPExp { _quant = quant', _oDecls = oDecls', _bpexp = bpexp' }) = do
    when (not $ null oDecls') $ unsupported "decls in quantified expression"
    case quant' of
        INo -> do
            name <- pExpFeature ctx bpexp'
            return $ CeOp "not" [CeFeat name]
        ILone -> do
            -- `lone` (set size either 0 or 1) is satisfied by every feature in
            -- the FMJSON output.  We only need to check that `bpexp'` resolves
            -- to a feature.
            void $ pExpFeature ctx bpexp'
            return $ CeLit True
        -- `one` (== 1) and `some` (>= 1) are equivalent when the cardinality
        -- of every resolvable feature is capped at 1.
        IOne -> CeFeat <$> pExpFeature ctx bpexp'
        ISome -> CeFeat <$> pExpFeature ctx bpexp'
        _ -> unsupported $ "quantifier " ++ show quant'
iExpConstraint ctx (IFunExp { _op = op', _exps = exps' })
  | op' `elem` ["=", "!="]
  , [ae, be] <- exps'
  , IInt a <- ae ^. exp
  , IInt b <- be ^. exp =
    return $ CeLit $ case op' of
        "=" -> a == b
        "!=" -> a /= b
        _ -> error "unreachable"
  | otherwise = do
    let requireArgCount n = when (length exps' /= n) $
            unsupported $ "operation " ++ show op' ++ " on " ++ show (length exps') ++
                " arguments (expected " ++ show n ++ ")"
    ceOp <- case op' of
        "&&" -> return "and"
        "||" -> return "or"
        "=>" -> requireArgCount 2 >> return "imp"
        "<=>" -> requireArgCount 2 >> return "eqv"
        "xor" -> return "xor"
        "!" -> requireArgCount 2 >> return "not"
        _ -> unsupported $ "operation " ++ show op'
    ceArgs <- mapM (\pe -> iExpConstraint ctx $ pe ^. exp) exps'
    return $ CeOp ceOp ceArgs
iExpConstraint _ctx e = unsupported $ "expression " ++ show e

pExpConstraint :: ConstraintContext -> PExp -> Either String CExp
pExpConstraint ctx e = case iExpConstraint ctx $ e ^. exp of
    Left err -> Left $ err ++ " (in constraint at " ++ show (e ^. inPos) ++ ")"
    Right x -> Right x


type ResolveMap = Map UID (Unique String)

-- Build the UID-to-UClafer resolution tables for `ucfr` and its descendants.
buildResolveM :: UClafer -> Writer (Map String ResolveMap) ResolveMap
buildResolveM ucfr = do
    let selfMap = M.fromList [(cfr ^. uid, One $ ucfr ^. unrolledUid)
            | cfr <- (ucfr ^. concCfr) : (ucfr ^. absCfrs)]
    childrenMap <- M.unionsWith (<>) <$> mapM buildResolveM (ucfr ^. children)
    let rm = M.unionWith (<>) selfMap childrenMap
    tell $ M.singleton (ucfr ^. unrolledUid) rm
    return rm

-- Build the UID-to-UClafer resolution tables for the entire model.  The first
-- return value is the map for the global scope; the second contains the map
-- for each `UClafer` in `ucfrs` and descendants, keyed on `unrolledUid`.
buildResolve :: [UClafer] -> (ResolveMap, Map String ResolveMap)
buildResolve ucfrs = runWriter $ M.unionsWith (<>) <$> mapM buildResolveM ucfrs

buildScopes :: Map String ResolveMap -> Map String String -> Map String Scope
buildScopes rms ps =
    M.mapWithKey (\name rm -> Scope (M.lookup name ps) rm) rms

elementAsPExp :: IElement -> Maybe PExp
elementAsPExp (IEConstraint True pexp) = Just pexp
elementAsPExp _ = Nothing

elementsConstraints :: ConstraintContext -> [IElement] -> Either String [CExp]
elementsConstraints ctx elts = do
    let exps' = mapMaybe elementAsPExp elts
    mapM (pExpConstraint ctx) exps'

uClaferConstraints :: ConstraintContext -> UClafer -> Either String [CExp]
uClaferConstraints ctx ucfr = do
    let elts = collectElements ucfr
    let ctx' = ctx & ctxCurName .~ Just (ucfr ^. unrolledUid)
    elementsConstraints ctx' elts


simplifyCExp :: CExp -> CExp
simplifyCExp (CeOp op' args)
  | isAssoc = CeOp op' $ concatMap (subArgs op') args'
  | otherwise = CeOp op' args'
  where
    isAssoc = op' `elem` ["and", "or", "xor"]
    args' = map simplifyCExp args
simplifyCExp (CeLit b) = CeLit b
simplifyCExp (CeFeat n) = CeFeat n

subArgs :: String -> CExp -> [CExp]
subArgs op' (CeOp op'' args') | op'' == op' = args'
subArgs _ x = [x]


collectElements :: UClafer -> [IElement]
collectElements ucfr = concatMap (\cfr -> cfr ^. elements) (ucfr ^. concCfr : ucfr ^. absCfrs)

-- Check for unsupported `IElement` variants.  Does not check for validity of
-- the wrapped clafer or constraint.
validateElement :: IElement -> Either String ()
validateElement (IEClafer _) = return ()
validateElement (IEConstraint isHard' pexp) = do
    when (not isHard') $ unsupported $ "assertion (at " ++ show (pexp ^. inPos) ++ ")"
validateElement (IEGoal _ pexp) = do
    unsupported $ "optimization goal (at " ++ show (pexp ^. inPos) ++ ")"

validateUClafer :: UClafer -> Either String ()
validateUClafer ucfr = do
    mapM_ validateElement $ collectElements ucfr

walkUClafer :: UClafer -> [UClafer]
walkUClafer ucfr = ucfr : concatMap walkUClafer (ucfr ^. children)

-- Build parent map entries for a single UClafer.  This maps the name of each
-- of `ucfr`'s children to the name of `ucfr` itself.
makeParentMap :: UClafer -> Map String String
makeParentMap ucfr = M.fromList $
    [(child ^. unrolledUid, ucfr ^. unrolledUid) | child <- ucfr ^. children]

uclaferFeature :: Map String String -> UClafer -> Feature
uclaferFeature parentMap ucfr = Feature
    (ucfr ^. unrolledUid)
    (convCard $ ucfr ^. concCfr . card)
    (convGCard $ ucfr ^. concCfr . gcard)
    (M.lookup (ucfr ^. unrolledUid) parentMap)
    (map (\c -> c ^. unrolledUid) $ ucfr ^. children)

iModuleToFMJSON :: IModule -> Either String A.Value
iModuleToFMJSON imod = do
    let uidMap = createUidIClaferMap imod
    let elts = imod ^. mDecls
    mapM_ validateElement elts
    ucfrs <- catMaybes <$> mapM (unrollElementClafer uidMap) elts
    let ucfrs' = assignUids ucfrs

    let allUCfrs = concatMap walkUClafer ucfrs'
    mapM_ validateUClafer allUCfrs
    let parentMap = foldMap makeParentMap allUCfrs
    let feats = map (uclaferFeature parentMap) allUCfrs
    let rootNames = filter (\n -> not $ M.member n parentMap) $ map (\f -> f ^. fName) feats

    -- `buildResolve` works by tree traversal, since we need to know the number
    -- of instances of each IClafer within each UClafer and its descendants.
    let (globalResMap, ucfrResMaps) = buildResolve ucfrs'
    let scopes = buildScopes ucfrResMaps parentMap
    let globalCtx = ConstraintContext Nothing scopes globalResMap
    gCExps <- elementsConstraints globalCtx elts
    ucCExps <- concat <$> mapM (uClaferConstraints globalCtx) allUCfrs

    return $ A.object
        [ "features" A..= M.fromList [(f ^. fName, f) | f <- feats]
        , "roots" A..= rootNames
        , "constraints" A..= map simplifyCExp (gCExps ++ ucCExps)
        ]
