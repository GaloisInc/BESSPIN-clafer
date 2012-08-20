{-
 Copyright (C) 2012 Kacper Bak, Jimmy Liang <http://gsd.uwaterloo.ca>

 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files (the "Software"), to deal in
 the Software without restriction, including without limitation the rights to
 use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 of the Software, and to permit persons to whom the Software is furnished to do
 so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
-}
module Language.Clafer.Intermediate.ResolverName where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Language.Clafer.Common
import Language.Clafer.Front.Absclafer
import Language.Clafer.Intermediate.Intclafer
import qualified Language.Clafer.Intermediate.Intclafer as I

data SEnv = SEnv {
  clafers :: [IClafer],                 -- (constant) top level clafers
  context :: Maybe IClafer,             -- context of a constraint
  subClafers :: [(IClafer, [IClafer])], -- descendans (BFS)
  ancClafers :: [(IClafer, [IClafer])], -- ancesors (BFS)
  bindings :: [([String], [IClafer])],  -- local names
  resPath :: [IClafer],                 -- path to the current clafer
  genv :: GEnv,                         -- (constant)
  aClafers :: [(IClafer, [IClafer])],   -- (constant) abstract clafers (BFS)
  cClafers :: [(IClafer, [IClafer])]    -- (constant) all concrete clafers (BFS)
  } deriving Show

data HowResolved =
    Special     -- "this", "parent", "children"
  | TypeSpecial -- primitive type: integer, string
  | Binding     -- local variable (in constraints)
  | Subclafers  -- clafer's descendant
  | Reference   -- resolved by a reference
  | Ancestor    -- clafer's ancestor
  | AbsClafer   -- abstract clafer
  | TopClafer   -- non-abstract top-level clafer
  deriving (Eq, Show)


defSEnv genv declarations = env {aClafers = rCl aClafers',
                                 cClafers = rCl cClafers'}
  where
  env = SEnv (toClafers declarations) Nothing [] [] [] [] genv [] []
  rCl cs = bfs toNodeDeep $ map (\c -> env{context = Just c, resPath = [c]}) cs
  (aClafers', cClafers') = partition isAbstract $ clafers env


resolveModuleNames :: (IModule, GEnv) -> IModule
resolveModuleNames (imodule, genv) =
  imodule{mDecls = map (resolveElement (defSEnv genv decls)) decls}
  where
  decls = mDecls imodule


resolveClafer :: SEnv -> IClafer -> IClafer
resolveClafer env clafer =
  clafer {elements = map (resolveElement env'{subClafers = subClafers',
                                              ancClafers = ancClafers'}) $
          elements clafer}
  where
  env' = env {context = Just clafer, resPath = clafer : resPath env}
  subClafers' = tail $ bfs toNodeDeep [env'{resPath = [clafer]}]
  ancClafers' = (init $ tails $ resPath env) >>= (mkAncestorList env)

mkAncestorList env rp =
  bfs toNodeDeep [env{context = Just $ head rp, resPath = rp}]

resolveElement :: SEnv -> IElement -> IElement
resolveElement env x = case x of
  IEClafer clafer  -> IEClafer $ resolveClafer env clafer
  IEConstraint isHard pexp  -> IEConstraint isHard $ resolvePExp env pexp
  IEGoal isMaximize pexp  -> IEGoal isMaximize $ resolvePExp env pexp  


resolvePExp :: SEnv -> PExp -> PExp
resolvePExp env pexp = pexp {Language.Clafer.Intermediate.Intclafer.exp = resolveIExp env $
                             Language.Clafer.Intermediate.Intclafer.exp pexp}

resolveIExp :: SEnv -> IExp -> IExp
resolveIExp env x = case x of
  IDeclPExp quant decls pexp -> IDeclPExp quant decls' $ resolvePExp env' pexp
    where
    (decls', env') = runState (mapM processDecl decls) env
  IFunExp op exps -> if op == iJoin then resNav else IFunExp op $ map res exps
  IInt n -> x
  IDouble n -> x
  IStr str -> x
  IClaferId _ _ _ -> resNav
  where
  res = resolvePExp env
  resNav = fst $ resolveNav env x True


processDecl decl = do
  env <- get
  let (body', path) = resolveNav env (Language.Clafer.Intermediate.Intclafer.exp $ body decl) True
  modify (\e -> e { bindings = (decls decl, path) : bindings e })
  return $ decl {body = pExpDefPidPos body'}

resolveNav :: SEnv -> IExp -> Bool -> (IExp, [IClafer])
resolveNav env x isFirst = case x of
  IFunExp _ (pexp0:pexp:_)  ->
    (IFunExp iJoin [pexp0{I.exp=exp0'}, pexp{I.exp=exp'}], path')
    where
    (exp0', path) = resolveNav env (Language.Clafer.Intermediate.Intclafer.exp pexp0) True
    (exp', path') = resolveNav env {context = listToMaybe path, resPath = path}
                    (Language.Clafer.Intermediate.Intclafer.exp pexp) False
  IClaferId modName id _ -> out
    where
    out
      | isFirst   = mkPath env $ resolveName env id
      | otherwise = mkPath' modName $ resolveImmName env id
  x -> error $ "Cannot resolve nav of " ++ show x 

mkPath :: SEnv -> (HowResolved, String, [IClafer]) -> (IExp, [IClafer])
mkPath env (howResolved, id, path) = case howResolved of
  Binding -> (mkLClaferId id True, path)
  Special -> (specIExp, path)
  TypeSpecial -> (mkLClaferId id True, path)
  Subclafers -> (toNav $ tail $ reverse $ map uid path, path)
  Ancestor -> (toNav' $ adjustAncestor (reverse $ map uid $ resPath env)
                                       (reverse $ map uid path), path)
  _ -> (toNav' $ reverse $ map uid path, path)
  where
  toNav = foldl
          (\exp id -> IFunExp iJoin [pExpDefPidPos exp, mkPLClaferId id False])
          (mkLClaferId this True)
  specIExp = if id /= this then toNav [id] else mkLClaferId id True

toNav' p = (mkIFunExp iJoin $ map (\c -> mkLClaferId c False) p) :: IExp


adjustAncestor :: [String] -> [String] -> [String]
adjustAncestor cPath rPath = this : parents ++ (fromJust $ stripPrefix prefix rPath)
  where
  parents = replicate (length $ fromJust $ stripPrefix prefix cPath) parent
  prefix  = fst $ unzip $ takeWhile (uncurry (==)) $ zip cPath rPath


mkPath' modName (howResolved, id, path) = case howResolved of
  Reference  -> (toNav' ["ref", id], path)
  _ -> (IClaferId modName id False, path)

-- -----------------------------------------------------------------------------

resolveName :: SEnv -> String -> (HowResolved, String, [IClafer])
resolveName env id = resolve env id
  [resolveSpecial, resolveBind, resolveDescendants, resolveAncestor, resolveTopLevel, resolveNone]


resolveImmName :: SEnv -> String -> (HowResolved, String, [IClafer])
resolveImmName env id = resolve env id
  [resolveSpecial, resolveChildren, resolveReference, resolveNone]


resolve env id fs = fromJust $ foldr1 mplus $ map (\x -> x env id) fs


-- reports error if clafer not found
resolveNone env id = error $ "resolver: " ++ id ++ " not found" ++
  " within " ++ (showPath $ map uid $ resPath env)


-- checks if ident is one of special identifiers
resolveSpecial :: SEnv -> String -> Maybe (HowResolved, String, [IClafer])
resolveSpecial env id
  | id `elem` [this, children, ref] =
      Just (Special, id, resPath env)
  | id == parent   = Just (Special, id, tail $ resPath env)
  | isPrimitive id = Just (TypeSpecial, id, [])
  | otherwise      = Nothing 


-- checks if ident is bound locally
resolveBind :: SEnv -> String -> Maybe (HowResolved, String, [IClafer])
resolveBind env id = find (\bs -> id `elem` fst bs) (bindings env) >>=
  (\x -> Just (Binding, id, snd x))


-- searches for a name in all subclafers (BFS)
resolveDescendants :: SEnv -> String -> Maybe (HowResolved, String, [IClafer])
resolveDescendants env id =
  (context env) >> (findFirst id $ subClafers env) >>= (toMTriple Subclafers)


-- searches for a name in immediate subclafers (BFS)
resolveChildren :: SEnv -> String -> Maybe (HowResolved, String, [IClafer])
resolveChildren env id = resolveChildren' env id allInhChildren Subclafers

-- searches for a name by dereferencing clafer
resolveReference :: SEnv -> String -> Maybe (HowResolved, String, [IClafer])
resolveReference env id = resolveChildren' env id allChildren Reference

resolveChildren' env id f label =
  (context env) >> (findUnique id $ map (\x -> (x, [x,fromJust $ context env]))
                    $ f env) >>= toMTriple label


resolveAncestor :: SEnv -> String -> Maybe (HowResolved, String, [IClafer])
resolveAncestor env id = context env >> (findUnique id $ ancClafers env) >>=
                         toMTriple Ancestor


-- searches for a feature starting from local root (BFS) and then continues with
-- other declarations
resolveTopLevel :: SEnv -> String -> Maybe (HowResolved, String, [IClafer])
resolveTopLevel env id = foldr1 mplus $ map
  (\(cs, hr) -> findUnique id cs >>= toMTriple hr)
  [(aClafers env, AbsClafer), (cClafers env, TopClafer)]


toNodeDeep env
  | length (clafer `elemIndices` resPath env) > 1 = (result, [])
  | otherwise = (result, map (\c -> env {context = Just c,
                                         resPath = c : resPath env}) $
                 allInhChildren env)
  where
  result = (clafer, resPath env)
  clafer = fromJust $ context env
  

allInhChildren = selectChildren getSuperNoArr

allChildren = selectChildren getSuper

selectChildren f env = getSubclafers $ concat $
                       mapHierarchy elements f (sClafers $ genv env)
                       (fromJust $ context env)

findUnique :: String -> [(IClafer, [IClafer])] -> Maybe (String, [IClafer])
findUnique x xs =
  case filterPaths x $ nub xs of
    []     -> Nothing
    [elem] -> Just $ (uid $ fst elem, snd elem)
    xs'    -> error $ "clafer " ++ show x ++ " " ++ errMsg
      where
      xs''   = map ((map uid).snd) xs'
      errMsg = (if isNamespaceConflict xs''
               then "cannot be defined because the name should be unique in the same namespace.\n"
               else "is not unique. ") ++ 
               "Available paths:\n" ++ (xs'' >>= showPath)

findFirst :: String -> [(IClafer, [IClafer])] -> Maybe (String, [IClafer])
findFirst x xs =
  case filterPaths x $ nub xs of
    []       -> Nothing
    (elem:_) -> Just $ (uid $ fst elem, snd elem)

showPath xs = (intercalate "." $ reverse xs) ++ "\n"

isNamespaceConflict (xs:ys:_) = tail xs == tail ys
isNamespaceConflict x         = error $ "isNamespaceConflict must be given a list"
                                         ++ " of at least two elements, but was given " ++ show x

filterPaths x xs = filter (((==) x).ident.fst) xs
