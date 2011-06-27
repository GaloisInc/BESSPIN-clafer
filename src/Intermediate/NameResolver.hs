module Intermediate.NameResolver where

import List
import Monad
import Data.Maybe
import Control.Monad.State

import Common
import Front.Absclafer
import Intermediate.Intclafer

data SEnv = SEnv {
  clafers :: [IClafer],
  context :: Maybe IClafer,
  bindings :: [String]
  } deriving Show


resolveDeclaration :: IModule -> IDeclaration -> IDeclaration
resolveDeclaration declarations x = case x of
  IClaferDecl clafer  -> IClaferDecl $ resolveClafer env clafer
  IConstDecl constraint  -> IConstDecl $ resolveLExp env constraint
  where
  env = SEnv (toClafers declarations) Nothing []


resolveClafer :: SEnv -> IClafer -> IClafer
resolveClafer env clafer =
  clafer {elements = map (resolveElement env {context = Just clafer}) $
                     elements clafer}


resolveSuper :: SEnv -> ISuper -> ISuper
resolveSuper env x = case x of
  ISuper True sexp -> ISuper True $ map (resolveSExp env) sexp
  _ -> x


resolveElement :: SEnv -> IElement -> IElement
resolveElement env x = case x of
  ISubclafer clafer  -> ISubclafer $ resolveClafer env clafer
  ISubconstraint constraint  -> ISubconstraint $ resolveLExp env constraint


resolveLExp :: SEnv -> ILExp -> ILExp
resolveLExp env x = case x of
  IEIff lexp0 lexp -> IEIff (res lexp0) (res lexp)
  IEImpliesElse lexp0 lexp Nothing -> IEImpliesElse (res lexp0) (res lexp) Nothing
  IEImpliesElse lexp0 lexp1 (Just lexp) -> IEImpliesElse (res lexp0) (res lexp1) (Just $ res lexp)
  IEOr lexp0 lexp -> IEOr (res lexp0) (res lexp)
  IEXor lexp0 lexp -> IEXor (res lexp0) (res lexp)
  IEAnd lexp0 lexp -> IEAnd (res lexp0) (res lexp)
  IENeg lexp -> IENeg (res lexp)
  IETerm term -> IETerm (resolveTerm env term)
  where
  res = resolveLExp env


resolveTerm :: SEnv -> ITerm -> ITerm
resolveTerm env x = case x of
  ITermCmpExp cmpexp -> ITermCmpExp $ resolveCmpExp env cmpexp
  ITermQuantSet quant sexp -> ITermQuantSet quant $ resolveSExp env sexp
  ITermQuantDeclExp decls lexp -> ITermQuantDeclExp
    decls' (resolveLExp env' lexp)
    where
    (decls', env') = runState (mapM processDecl decls) env


processDecl decl = do
  env <- get
  modify (\e -> e { bindings = getIdents decl ++ bindings e })
  return $ resolveDecl env decl


getIdents (Decl _ _ locids _) = map
  (\(LocIdIdent ident) -> transIdent ident) locids


resolveDecl :: SEnv -> Decl -> Decl
resolveDecl env x = case x of
  Decl exquant disj locids sexp -> Decl exquant disj locids $
                                   resolveSExp env sexp

resolveCmpExp :: SEnv -> CmpExp -> CmpExp
resolveCmpExp env x = case x of
  ELt exp0 exp  -> ELt (res exp0) (res exp)
  EGt exp0 exp  -> EGt (res exp0) (res exp)
  EREq exp0 exp -> EREq (res exp0) (res exp)
  EEq exp0 exp  -> EEq (res exp0) (res exp)
  ELte exp0 exp  -> ELte (res exp0) (res exp)
  EGte exp0 exp  -> EGte (res exp0) (res exp)
  ENeq exp0 exp  -> ENeq (res exp0) (res exp)
  ERNeq exp0 exp -> ERNeq (res exp0) (res exp)
  EIn exp0 exp   -> EIn (res exp0) (res exp)
  ENin exp0 exp  -> ENin (res exp0) (res exp)
  where
  res = resolveExp env


resolveExp :: SEnv -> Exp -> Exp
resolveExp env x = case x of
  ESetExp  sexp -> ESetExp $ resolveSExp env sexp
  ENumExp aexp -> ENumExp $ resolveAExp env aexp
  EStrExp strexp -> x


resolveSExp :: SEnv -> SExp -> SExp
resolveSExp env x = case x of
  SExpUnion sexp0 sexp -> SExpUnion (res sexp0) (res sexp)
  SExpIntersection sexp0 sexp  -> SExpIntersection (res sexp0) (res sexp)
  SExpDomain sexp0 sexp  -> SExpDomain (res sexp0) (res sexp)
  SExpRange sexp0 sexp  -> SExpRange (res sexp0) (res sexp)
  SExpJoin sexp0 sexp  -> snd $ resolveNav env x True
  SExpIdent id -> snd $ resolveNav env x True
  where
  res = resolveSExp env


resolveNav :: SEnv -> SExp -> Bool -> (Maybe IClafer, SExp)
resolveNav env x isFirst = case x of
  SExpJoin sexp0 sexp  -> (context'', SExpJoin sexp0' sexp')
    where
    (context', sexp0') = resolveNav env sexp0 True
    (context'', sexp') = resolveNav env {context = context'} sexp False
  SExpIdent id -> (findName env id', SExpIdent $ Ident $ id')
    where
    id' = (if isFirst then resolveName else resolveImmName) env $ transIdent id

-- -----------------------------------------------------------------------------
-- analyzes arithmetic expression
resolveAExp :: SEnv -> AExp -> AExp
resolveAExp env x = case x of
  EAdd aexp0 aexp -> EAdd (res aexp0) (res aexp)
  ESub aexp0 aexp -> ESub (res aexp0) (res aexp)
  EMul aexp0 aexp -> EMul (res aexp0) (res aexp)
  EUmn aexp       -> EUmn (res aexp)
  ECSetExp sexp   -> ECSetExp $ resolveSExp env sexp
  EInt n -> x
  where
  res = resolveAExp env

-- -----------------------------------------------------------------------------

resolveName :: SEnv -> String -> String
resolveName env id = resolve env id
  [resolveSpecial, resolveBind, resolveSubclafers, resolveTopLevel, resolveNone]


resolveImmName :: SEnv -> String -> String
resolveImmName env id = resolve env id
  [resolveImmSubclafers, resolveNone]


resolve env id fs = fromJust $ foldr1 mplus $ map (\x -> x env id) fs


-- reports error if clafer not found
resolveNone env id = error $ id ++ " not found within " ++
  (show $ context env >>= (Just . ident))


-- checks if ident is one of special identifiers
resolveSpecial :: SEnv -> String -> Maybe String
resolveSpecial _ id
  | id `elem` [this, strType, intType, integerType, parent, children] = Just id
  | otherwise                                                         = Nothing 


-- checks if ident is bound locally
resolveBind :: SEnv -> String -> Maybe String
resolveBind env id = find (== id) (bindings env)


-- searches for a name in all subclafers (BFS)
resolveSubclafers :: SEnv -> String -> Maybe String
resolveSubclafers env id =
  (context env) >> (findUnique id $ tail $ bfs toNodeDeep [env])


-- searches for a name in immediate subclafers (BFS)
resolveImmSubclafers :: SEnv -> String -> Maybe String
resolveImmSubclafers env id =
  (context env) >> (findUnique id $ allSubclafers env)


-- searches for a feature starting from local root (BFS) and then continues with
-- other declarations
resolveTopLevel :: SEnv -> String -> Maybe String
resolveTopLevel env id = case context env of
  Nothing -> resolvePath $ clafers env
  _  -> foldr1 mplus $ map resolvePath $ pairToList $ partition
        (isEqClaferId (last $ findPath (uid $ fromJust $ context env) $
                       clafers env)) $ clafers env
  where
  resolvePath xs = findUnique id $ bfs toNodeDeep $
                   map (\c -> env {context = Just c}) xs


toNodeDeep env = (fromJust $ context env,
                  map (\c -> env {context = Just c}) $ allSubclafers env)


allSubclafers env = getSubclafers $ concat $
                    mapHierarchy elements (clafers env) (fromJust $ context env)


findPath :: String -> [IClafer] -> [String]
findPath id clafers = map uid $ resolvePath id [] clafers


resolvePath :: String -> [IClafer] -> [IClafer] -> [IClafer]
resolvePath id path clafers = if (not.null) path' then head path' else []
  where
  path' = filter (not.null) $ map (findClafer id path) clafers


findClafer :: String -> [IClafer] -> IClafer -> [IClafer]
findClafer id path clafer
  | id == uid clafer = clafer : path
  | otherwise        = path
  where
  path = resolvePath id (clafer : path) $ getSubclafers $ elements clafer


findName :: SEnv -> String -> Maybe IClafer
findName env id 
  | id == parent && context env /= Nothing && length ppath > 1 =
      Just $ head $ tail ppath
  | id `elem` [this, children, strType, intType, integerType] = context env
  | (not.null) path                              = Just $ head path
  | otherwise                                    = resolveNone env id
  where
  path  = resolvePath id [] $ clafers env
  ppath = resolvePath (uid $ fromJust $ context env) [] $ clafers env


findUnique :: String -> [IClafer] -> Maybe String
findUnique x xs =
  case filter (((==) x).ident) xs of
    []     -> Nothing
    [elem] -> Just $ uid elem
    _      -> error $ "element is not unique : " ++ show x
