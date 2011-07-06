module Intermediate.ResolverName where

import List
import Monad
import Data.Maybe
import Control.Monad.State
import Data.Function

import Common
import Front.Absclafer
import Intermediate.Intclafer

data SEnv = SEnv {
  clafers :: [IClafer],
  context :: Maybe IClafer,
  bindings :: [([String], [IClafer])],
  resPath :: [IClafer],
  genv :: GEnv
  } deriving Show


data HowResolved = Special | TypeSpecial | Binding | Subclafers | Ancestor | AbsClafer | TopClafer
  deriving (Eq, Show)


resolveModuleNames :: (IModule, GEnv) -> IModule
resolveModuleNames (declarations, genv) =
  map (resolveDeclaration (declarations, genv)) declarations

resolveDeclaration :: (IModule, GEnv) -> IDeclaration -> IDeclaration
resolveDeclaration (declarations, genv) x = case x of
  IClaferDecl clafer  -> IClaferDecl $ resolveClafer env clafer
  IConstDecl constraint  -> IConstDecl $ resolveLExp env constraint
  where
  env = SEnv (toClafers declarations) Nothing [] [] genv


resolveClafer :: SEnv -> IClafer -> IClafer
resolveClafer env clafer =
  clafer {elements = map (resolveElement env
  {context = Just clafer, resPath = clafer : resPath env}) $ elements clafer}


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
  ITermCmpExp cmpexp t -> ITermCmpExp (resolveCmpExp env cmpexp) t
  ITermQuantSet quant sexp -> ITermQuantSet quant $ resolveSExp env sexp
  ITermQuantDeclExp decls lexp -> ITermQuantDeclExp
    decls' (resolveLExp env' lexp)
    where
    (decls', env') = runState (mapM processDecl decls) env


processDecl decl = do
  env <- get
  let (body', path) = resolveNav env (body decl) True
  modify (\e -> e { bindings = (decls decl, path) : bindings e })
  return $ decl {body = body'}


resolveCmpExp :: SEnv -> ICmpExp -> ICmpExp
resolveCmpExp env x = case x of
  IELt exp0 exp  -> on IELt res exp0 exp
  IEGt exp0 exp  -> on IEGt res exp0 exp
  IEREq exp0 exp -> on IEREq res exp0 exp
  IEEq exp0 exp  -> on IEEq res exp0 exp
  IELte exp0 exp  -> on IELte res exp0 exp
  IEGte exp0 exp  -> on IEGte res exp0 exp
  IENeq exp0 exp  -> on IENeq res exp0 exp
  IERNeq exp0 exp -> on IERNeq res exp0 exp
  IEIn exp0 exp   -> on IEIn res exp0 exp
  IENin exp0 exp  -> on IENin res exp0 exp
  where
  res =  resolveExp env


resolveExp :: SEnv -> IExp -> IExp
resolveExp env x = case x of
  IESetExp  sexp -> IESetExp $ resolveSExp env sexp
  IENumExp aexp -> IENumExp $ resolveAExp env aexp
  IEStrExp strexp -> x


resolveSExp :: SEnv -> ISExp -> ISExp
resolveSExp env x = case x of
  ISExpUnion sexp0 sexp -> on ISExpUnion res sexp0 sexp
  ISExpIntersection sexp0 sexp  -> on ISExpIntersection res sexp0 sexp
  ISExpDomain sexp0 sexp  -> on ISExpDomain res sexp0 sexp
  ISExpRange sexp0 sexp  -> on ISExpRange res sexp0 sexp
  _  -> fst $ resolveNav env x True
  where
  res = resolveSExp env


resolveNav :: SEnv -> ISExp -> Bool -> (ISExp, [IClafer])
resolveNav env x isFirst = case x of
  ISExpJoin sexp0 sexp  -> (ISExpJoin sexp0' sexp', path')
    where
    (sexp0', path) = resolveNav env sexp0 True
    (sexp', path') =
        resolveNav env {context = listToMaybe path, resPath = path} sexp False
  ISExpIdent id _ -> out 
    where
    out
      | isFirst   = mkPath env $ resolveName env id
      | otherwise = (ISExpIdent (snd3 ctx) False, trd3 ctx)
    ctx = resolveImmName env id


mkPath :: SEnv -> (HowResolved, String, [IClafer]) -> (ISExp, [IClafer])
mkPath env (howResolved, id, path) = case howResolved of
  Binding -> (ISExpIdent id True, path)
  Special -> (ISExpIdent id True, path)
  TypeSpecial -> (ISExpIdent id True, path)
  Subclafers -> (toNav $ tail $ reverse $ map uid path, path)
  _ -> (toNav' $ reverse $ map uid path, path)
  where
  id'   = ISExpIdent id False
  toNav = foldl (\sexp id -> ISExpJoin sexp $ ISExpIdent id False)
          (ISExpIdent this True)
  toNav' p = mkNav $ map (\c -> ISExpIdent c $ isTopLevel c $ clafers env) p


isTopLevel id clafers = id `elem` (map uid clafers)


mkNav (x:[]) = x
mkNav xs = foldl1 ISExpJoin xs


-- -----------------------------------------------------------------------------
-- analyzes arithmetic expression
resolveAExp :: SEnv -> IAExp -> IAExp
resolveAExp env x = case x of
  IEAdd aexp0 aexp -> on IEAdd res aexp0 aexp
  IESub aexp0 aexp -> on IESub res aexp0 aexp
  IEMul aexp0 aexp -> on IEMul res aexp0 aexp
  IEUmn aexp       -> IEUmn $ res aexp
  IECSetExp sexp   -> IECSetExp $ resolveSExp env sexp
  IEInt n -> x
  where
  res = resolveAExp env

-- -----------------------------------------------------------------------------

resolveName :: SEnv -> String -> (HowResolved, String, [IClafer])
resolveName env id = resolve env id
  [resolveSpecial, resolveBind, resolveSubclafers, resolveTopLevel, resolveNone]


resolveImmName :: SEnv -> String -> (HowResolved, String, [IClafer])
resolveImmName env id = resolve env id
  [resolveImmSubclafers, resolveNone]


resolve env id fs = fromJust $ foldr1 mplus $ map (\x -> x env id) fs


-- reports error if clafer not found
resolveNone env id = error $ id ++ " not found within " ++
  (show $ context env >>= (Just . ident)) ++ show env


-- checks if ident is one of special identifiers
resolveSpecial :: SEnv -> String -> Maybe (HowResolved, String, [IClafer])
resolveSpecial env id
  | id `elem` [this, children] =
      Just (Special, id, (fromJust $ context env) : resPath env)
  | id == parent = Just (Special, id, resPath env)
  | id `elem` [strType, intType, integerType] = Just (TypeSpecial, id, [])
  | otherwise                                 = Nothing 


-- checks if ident is bound locally
resolveBind :: SEnv -> String -> Maybe (HowResolved, String, [IClafer])
resolveBind env id = find (\bs -> id `elem` fst bs) (bindings env) >>=
  (\x -> Just (Binding, id, snd x))


-- searches for a name in all subclafers (BFS)
resolveSubclafers :: SEnv -> String -> Maybe (HowResolved, String, [IClafer])
resolveSubclafers env id = 
  (context env) >> (findUnique id $ tail $ bfs toNodeDeep
                    [env {resPath = [fromJust $ context env]}]) >>=
                    (toMTriple Subclafers)


-- searches for a name in immediate subclafers (BFS)
resolveImmSubclafers :: SEnv -> String -> Maybe (HowResolved, String, [IClafer])
resolveImmSubclafers env id =
  (context env) >> (findUnique id $ map (\x -> (x, [x,fromJust $ context env]))
                    $ allSubclafers env) >>= toMTriple Subclafers


-- searches for a feature starting from local root (BFS) and then continues with
-- other declarations
resolveTopLevel :: SEnv -> String -> Maybe (HowResolved, String, [IClafer])
resolveTopLevel env id = foldr1 mplus $ map
  (\(cs, hr) -> resolveUnique cs >>= toMTriple hr)
  [(ancestor, Ancestor), (aClafers, AbsClafer), (cClafers, TopClafer)]
  where
  (ancestor, topLevel) = partition (isAncestor env) $ clafers env
  (aClafers, cClafers) = partition isAbstract topLevel
  resolveUnique cs = findUnique id $ bfs toNodeDeep $
                     map (\c -> env {context = Just c, resPath = [c]}) cs


isAncestor env cs = (not $ isNothing $ context env) &&
  isEqClaferId (uid $ last $ (fromJust $ context $ env) : resPath env) cs


toNodeDeep env
  | length (clafer `elemIndices` resPath env) > 1 = (result, [])
  | otherwise = (result, map (\c -> env {context = Just c,
                                         resPath = c : resPath env}) $
                 allSubclafers env)
  where
  result = (clafer, resPath env)
  clafer = fromJust $ context env
  

allSubclafers env = getSubclafers $ concat $
                    mapHierarchy elements (clafers env) (fromJust $ context env)


findUnique :: String -> [(IClafer, [IClafer])] -> Maybe (String, [IClafer])
findUnique x xs =
  case filter (((==) x).ident.fst) xs of
    []     -> Nothing
    [elem] -> Just $ (uid $ fst elem, snd elem)
    _      -> error $ "element is not unique : " ++ show x ++ ". Available paths: " ++ ((filter (((==) x).ident.fst) xs) >>= (show.(map uid).snd))
