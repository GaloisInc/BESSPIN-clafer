{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Language.Clafer.Generator.FMJSON where

import Prelude hiding (exp)
import Control.Lens hiding (elements, children)
import Control.Monad
import Control.Monad.State
import qualified Data.Aeson as A
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
    , _fChildren :: [Feature]
    }

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
        , "children" A..= (f ^. fChildren)
        ]


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

uclaferFeature :: UClafer -> Either String Feature
uclaferFeature ucfr = do
    mapM_ validateElement $ collectElements ucfr
    children' <- mapM uclaferFeature $ ucfr ^. children
    return $ Feature
        (ucfr ^. unrolledUid)
        (convCard $ ucfr ^. concCfr . card)
        (convGCard $ ucfr ^. concCfr . gcard)
        children'


iModuleToFMJSON :: IModule -> Either String A.Value
iModuleToFMJSON imod = do
    let uidMap = createUidIClaferMap imod
    let elts = imod ^. mDecls
    mapM_ validateElement elts
    ucfrs <- catMaybes <$> mapM (unrollElementClafer uidMap) elts
    let ucfrs' = assignUids ucfrs
    feats <- mapM uclaferFeature ucfrs'

    return $ A.object
        [ "features" A..= feats
        ]
