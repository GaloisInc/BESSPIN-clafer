{-
 This file is part of the Clafer Translator (clafer).

 Copyright (C) 2010 Kacper Bak <http://gsd.uwaterloo.ca/kbak>

 clafer is free software: you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 clafer is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public License
 along with clafer. (See files COPYING and COPYING.LESSER.)  If not,
 see <http://www.gnu.org/licenses/>.
-}
module Intermediate.Desugarer where

import Front.Absclafer
import Intermediate.Intclafer
import Monad
import Common hiding (Clafer)
import Data.Function

desugarModule :: Module -> IModule
desugarModule x = case x of
  Module declarations  -> IModule "" $
      map desugarDeclaration (declarations >>= desugarEnums)


sugarModule :: IModule -> Module
sugarModule x = Module $ map sugarDeclaration $ mDecls x


-- desugars enumeration to abstract and global singleton features
desugarEnums :: Declaration -> [Declaration]
desugarEnums x = case x of
  EnumDecl id enumids  -> absEnum : map mkEnum enumids
    where
    absEnum = ClaferDecl $ Clafer
              Abstract GCardEmpty id SuperEmpty CardEmpty InitEmpty ElementsEmpty
    mkEnum (EnumIdIdent eId) = ClaferDecl $ Clafer AbstractEmpty GCardEmpty
                                  eId (SuperSome SuperHow_1 (ClaferId $ LocClafer id)) CardEmpty InitEmpty ElementsEmpty
  _ -> [x]


desugarDeclaration :: Declaration -> IElement
desugarDeclaration x = case x of
  EnumDecl id enumids  -> error "desugared"
  ClaferDecl clafer  -> IEClafer $ desugarClafer clafer
  ConstDecl constraint  -> IEConstraint True $ desugarConstraint constraint


sugarDeclaration :: IElement -> Declaration
sugarDeclaration x = case x of
  IEClafer clafer  -> ClaferDecl $ sugarClafer clafer
  IEConstraint _ constraint  -> ConstDecl $ sugarConstraint constraint


desugarClafer :: Clafer -> IClafer
desugarClafer x = case x of
  Clafer abstract gcard id super card init elements  -> 
    IClafer (desugarAbstract abstract) (desugarGCard gcard) (transIdent id)
            "" (desugarSuper super) (desugarCard card) (0, ExIntegerAst)
            ((desugarInit init) ++ desugarElements elements)


sugarClafer :: IClafer -> Clafer
sugarClafer x = case x of
  IClafer abstract gcard id uid super card _ elements  ->
    Clafer (sugarAbstract abstract) (sugarGCard gcard) (Ident uid)
      (sugarSuper super) (sugarCard card) InitEmpty (sugarElements elements)


desugarSuper :: Super -> ISuper
desugarSuper x = case x of
  SuperEmpty  -> ISuper False [PExp (Just ISet) "" $ mkLClaferId baseClafer True]
  SuperSome superhow setexp -> ISuper (desugarSuperHow superhow) [desugarSetExp setexp]


desugarSuperHow :: SuperHow -> Bool
desugarSuperHow x = case x of
  SuperHow_1  -> False
  SuperHow_2  -> True


desugarInit :: Init -> [IElement]
desugarInit x = case x of
  InitEmpty  -> []
  InitSome inithow exp  -> [IEConstraint (desugarInitHow inithow) (PExp Nothing "" (IFunExp "=" [mkPLClaferId "this" False, desugarExp exp]))]


desugarInitHow :: InitHow -> Bool
desugarInitHow x = case x of
  InitHow_1  -> True
  InitHow_2  -> False


desugarName (LocClafer id)      = desugarName (ModClafer [] id)
desugarName (ModClafer mods id) =
  IClaferId (concatMap ((++ modSep).desugarModId) mods) (transIdent id) True

desugarModId (ModIdIdent id) = transIdent id

sugarModId modid = ModIdIdent $ Ident modid


sugarSuper :: ISuper -> Super
sugarSuper x = case x of
  ISuper _ [] -> SuperEmpty
  ISuper isOverlapping [pexp] -> SuperSome (sugarSuperHow isOverlapping) (sugarSetExp pexp)


sugarSuperHow x = case x of
  False -> SuperHow_1
  True  -> SuperHow_2


sugarInitHow :: Bool -> InitHow
sugarInitHow x = case x of
  True  -> InitHow_1
  False -> InitHow_2


desugarConstraint :: Constraint -> PExp
desugarConstraint (Constraint exps) =
  desugarPath $ desugarExp $ (if length exps > 1 then foldl1 EAnd else head) exps


sugarConstraint :: PExp -> Constraint
sugarConstraint pexp = Constraint $ map sugarExp [pexp]


desugarAbstract :: Abstract -> Bool
desugarAbstract x = case x of
  AbstractEmpty  -> False
  Abstract  -> True


sugarAbstract :: Bool -> Abstract
sugarAbstract x = case x of
  False -> AbstractEmpty
  True -> Abstract


desugarElements :: Elements -> [IElement]
desugarElements x = case x of
  ElementsEmpty  -> []
  ElementsList elements  -> map desugarElement elements


sugarElements :: [IElement] -> Elements
sugarElements x = ElementsList $ map sugarElement x


desugarElement :: ElementCl -> IElement
desugarElement x = case x of
  Subclafer clafer  -> IEClafer $ desugarClafer clafer
  ClaferUse name card elements  -> IEClafer $ desugarClafer $ Clafer
      AbstractEmpty GCardEmpty (Ident $ sident $ desugarName name)
      (SuperSome SuperHow_1 (ClaferId name)) card InitEmpty elements
  Subconstraint constraint  -> IEConstraint True $ desugarConstraint constraint


sugarElement :: IElement -> ElementCl
sugarElement x = case x of
  IEClafer clafer  -> Subclafer $ sugarClafer clafer
  IEConstraint _ constraint -> Subconstraint $ sugarConstraint constraint


desugarGCard :: GCard -> Maybe IGCard
desugarGCard x = case x of
  GCardEmpty  -> Nothing
  GCardXor -> Just $ IGCard True (1, ExIntegerNum 1)
  GCardOr  -> Just $ IGCard True (1, ExIntegerAst)
  GCardMux -> Just $ IGCard True (0, ExIntegerNum 1)
  GCardOpt -> Just $ IGCard True (0, ExIntegerAst)
  GCardInterval (GNCard i ex)  -> Just $ IGCard False (i, ex)


sugarGCard :: Maybe IGCard -> GCard
sugarGCard x = case x of
  Nothing -> GCardEmpty
  Just (IGCard _ (i, ex)) -> GCardInterval $ GNCard i ex


desugarCard :: Card -> Maybe Interval
desugarCard x = case x of
  CardEmpty  -> Nothing
  CardLone  ->  Just (0, ExIntegerNum 1)
  CardSome  ->  Just (1, ExIntegerAst)
  CardAny  ->   Just (0, ExIntegerAst)
  CardNum n  -> Just (n, ExIntegerNum n)
  CardInterval (NCard i ex)  -> Just (i, ex)


sugarCard :: Maybe Interval -> Card
sugarCard x = case x of
  Nothing -> CardEmpty
  Just (i, ex) -> CardInterval $ NCard i ex


desugarExp :: Exp -> PExp
desugarExp x = PExp Nothing "" $ desugarExp' x

desugarExp' :: Exp -> IExp
desugarExp' x = case x of
  DeclExp exquant decls exp  ->
    IDeclPExp (desugarExQuant exquant) (map desugarDecl decls) (desugarExp exp)
  EIff exp0 exp  -> dop iIff [exp0, exp]
  EImplies exp0 exp  -> dop iImpl [exp0, exp]
  EImpliesElse exp0 exp1 exp  -> dop iIfThenElse [exp0, exp1, exp]
  EOr exp0 exp  -> dop iOr   [exp0, exp]
  EXor exp0 exp  -> dop iXor [exp0, exp]
  EAnd exp0 exp  -> dop iAnd [exp0, exp]
  ENeg exp  -> dop iNot [exp]
  QuantExp quant exp  -> IDeclPExp (desugarQuant quant) [] (desugarExp exp)
  ELt  exp0 exp  -> dop iLt  [exp0, exp]
  EGt  exp0 exp  -> dop iGt  [exp0, exp]
  EEq  exp0 exp  -> dop iEq  [exp0, exp]
  ELte exp0 exp  -> dop iLte [exp0, exp]
  EGte exp0 exp  -> dop iGte [exp0, exp]
  ENeq exp0 exp  -> dop iNeq [exp0, exp]
  EIn  exp0 exp  -> dop iIn  [exp0, exp]
  ENin exp0 exp  -> dop iNin [exp0, exp]
  EAdd exp0 exp  -> dop iPlus [exp0, exp]
  ESub exp0 exp  -> dop iSub [exp0, exp]
  EMul exp0 exp  -> dop iMul [exp0, exp]
  EDiv exp0 exp  -> dop iDiv [exp0, exp]
  ECSetExp exp   -> dop iCSet [exp]
  EMinExp exp    -> dop iMin [exp]
  EInt n  -> IInt n
  EDouble n -> IDouble n
  EStr str  -> IStr str
  ESetExp sexp -> desugarSetExp' sexp
  where
  dop = desugarOp desugarExp


desugarOp f op exps = IFunExp op $ map (trans.f) exps
  where
  trans = if op `elem` ([iNot, iIfThenElse] ++ logBinOps)
          then desugarPath else id

desugarSetExp :: SetExp -> PExp
desugarSetExp x = PExp Nothing "" $ desugarSetExp' x


desugarSetExp' :: SetExp -> IExp
desugarSetExp' x = case x of
  Union exp0 exp        -> dop iUnion        [exp0, exp]
  UnionCom exp0 exp     -> dop iUnion        [exp0, exp]
  Difference exp0 exp   -> dop iDifference   [exp0, exp]
  Intersection exp0 exp -> dop iIntersection [exp0, exp]
  Domain exp0 exp       -> dop iDomain       [exp0, exp]
  Range exp0 exp        -> dop iRange        [exp0, exp]
  Join exp0 exp         -> dop iJoin         [exp0, exp]
  ClaferId name  -> desugarName name
  where
  dop = desugarOp desugarSetExp


sugarExp :: PExp -> Exp
sugarExp x = sugarExp' $ Intermediate.Intclafer.exp x

sugarExp' :: IExp -> Exp
sugarExp' x = case x of
  IDeclPExp quant [] pexp -> QuantExp (sugarQuant quant) (sugarExp pexp)
  IDeclPExp quant decls pexp ->
    DeclExp (sugarExQuant quant) (map sugarDecl decls) (sugarExp pexp)
  IFunExp op exps ->
    if op `elem` unOps then (sugarUnOp op) (exps'!!0)
    else if op `elem` setBinOps then (ESetExp $ sugarSetExp' x)
    else if op `elem` binOps then (sugarOp op) (exps'!!0) (exps'!!1)
    else (sugarTerOp op) (exps'!!0) (exps'!!1) (exps'!!2)
    where
    exps' = map sugarExp exps
  IInt n -> EInt n
  IDouble n -> EDouble n
  IStr str -> EStr str  
  IClaferId _ _ _ -> ESetExp $ sugarSetExp' x
  where
  sugarUnOp op
    | op == iNot           = ENeg
    | op == iCSet          = ECSetExp
    | op == iMin           = EMinExp
  sugarOp op
    | op == iIff           = EIff
    | op == iImpl          = EImplies
    | op == iOr            = EOr
    | op == iXor           = EXor
    | op == iAnd           = EAnd 
    | op == iLt            = ELt
    | op == iGt            = EGt
    | op == iEq            = EEq  
    | op == iLte           = ELte
    | op == iGte           = EGte
    | op == iNeq           = ENeq
    | op == iIn            = EIn
    | op == iNin           = ENin
    | op == iPlus          = EAdd
    | op == iSub           = ESub
    | op == iMul           = EMul
    | op == iDiv           = EDiv
  sugarTerOp op
    | op == iIfThenElse    = EImpliesElse


sugarSetExp :: PExp -> SetExp
sugarSetExp x = sugarSetExp' $ Intermediate.Intclafer.exp x


sugarSetExp' :: IExp -> SetExp
sugarSetExp' x = case x of
  IFunExp op exps -> (sugarOp op) (exps'!!0) (exps'!!1)
    where
    exps' = map sugarSetExp exps
    sugarOp op
      | op == iUnion         = Union
      | op == iDifference    = Difference
      | op == iIntersection  = Intersection
      | op == iDomain        = Domain
      | op == iRange         = Range
      | op == iJoin          = Join
  IClaferId "" id _ -> ClaferId $ LocClafer $ Ident id
  IClaferId modName id _ -> ClaferId $ ModClafer [sugarModId modName] (Ident id)

desugarPath :: PExp -> PExp
desugarPath (PExp iType pid x) = PExp iType pid result
  where
  result
    | isPath x    = IDeclPExp ISome [] (PExp Nothing "" x)
    | isNegSome x = IDeclPExp INo   [] $ bpexp $ Intermediate.Intclafer.exp $ head $ exps x
    | otherwise   =  x
  isNegSome (IFunExp op [PExp _ _ (IDeclPExp ISome [] _)]) = op == iNot
  isNegSome _ = False


isPath :: IExp -> Bool
isPath (IClaferId _ _ _)  = True
isPath (IFunExp op _) = op == iJoin
isPath _ = False


desugarDecl :: Decl -> IDecl
desugarDecl x = case x of
  Decl disj locids exp  ->
    IDecl (desugarDisj disj) (map desugarLocId locids) (desugarExp exp)


sugarDecl :: IDecl -> Decl
sugarDecl x = case x of
  IDecl disj locids exp  ->
    Decl (sugarDisj disj) (map sugarLocId locids) (sugarExp exp)


desugarDisj :: Disj -> Bool
desugarDisj x = case x of
  DisjEmpty -> False
  Disj -> True


sugarDisj :: Bool -> Disj
sugarDisj x = case x of
  False -> DisjEmpty
  True -> Disj


desugarLocId :: LocId -> String
desugarLocId x = case x of
  LocIdIdent id  -> transIdent id


sugarLocId :: String -> LocId
sugarLocId x = LocIdIdent $ Ident x


desugarQuant x = case x of
  QuantNo -> INo
  QuantLone -> ILone
  QuantOne -> IOne
  QuantSome -> ISome


sugarQuant x = case x of
  INo -> QuantNo
  ILone -> QuantLone
  IOne -> QuantOne
  ISome -> QuantSome

desugarExQuant x = case x of
  ExQuantAll -> IAll
  ExQuantQuant quant -> desugarQuant quant

sugarExQuant x = case x of
  IAll -> ExQuantAll
  _ -> ExQuantQuant $ sugarQuant x
