{-# LANGUAGE NamedFieldPuns #-}

module Rule where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Monad.Except
import           Control.Monad.Identity (runIdentity)
import           Expression             (Expression (..))
import           Type                   (MonoType (..), PolyType (..),
                                         Typing (..), differenceContext,
                                         extendContext, getFirstContext,
                                         memberContext, memberTypeContext,
                                         memberNameContext, sizeContext)
import           Utils                  (isSpecializedType)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

data Rule
  = VarRule  { bottom :: Typing }
  | AppRule  { bottom :: Typing, topLeft :: Typing, topRight :: Typing }
  | AbsRule  { bottom :: Typing, top :: Typing }
  | LetRule  { bottom :: Typing, topLeft :: Typing, topRight :: Typing }
  | InstRule { bottom :: Typing, top :: Typing }
  | GenRule  { bottom :: Typing, top :: Typing }

----------------------------------------------------------------------
-- Checker
----------------------------------------------------------------------

checkRule :: Rule -> Except String Bool
checkRule VarRule  { bottom } = case bottom of
  Typing bCntx bVar@(Var _) bType -> do
    unless (memberContext bCntx bVar bType) (throwError $ show bVar ++ " : " ++ show bType ++ " ∉ Γ")
    return True
  _                               -> throwError "expression must be variable"

checkRule AppRule  { bottom, topLeft, topRight } = case bottom of
  Typing bCntx (Application bExprL bExprR) bType@(TMono _) -> do
    case (topLeft, topRight) of
      (Typing tlCntx tlExpr (TMono(tlFrom `TArrow` tlTo)), Typing trCntx trExpr trType@(TMono _)) -> do
        unless (bCntx == tlCntx) (throwError "Contexts are not same")
        unless (tlCntx == trCntx) (throwError "Contexts are not same")
        unless (tlExpr == bExprL) (throwError "Abstraction part of term is not same")
        unless (trExpr == bExprR) (throwError "Argument part of term is not same")
        unless (TMono tlFrom == trType) (throwError "AppRule -> tlFrom == trType")
        unless (TMono tlTo == bType) (throwError "AppRule -> tlTo == bType")
        return True
      _                                                                                 -> throwError "Type1 of abstraction is not in form σ → τ"
  _                                              -> throwError "Incorrect rule for this term"

checkRule AbsRule  { bottom, top } = case bottom of
  Typing bCntx (Lambda bName bExpr) (TMono(bFrom `TArrow` bTo)) ->
    case top of
      Typing tCntx tExpr tType@(TMono _) -> do
        let differenceContext1 = differenceContext tCntx bCntx
        unless (sizeContext differenceContext1 == 1) (throwError "Difference between top context and bottom context is not 1")
        let nameAndType = getFirstContext differenceContext1
        unless (extendContext bCntx nameAndType == tCntx) (throwError "Context -> AbsRule")
        unless (fst nameAndType == bName) (throwError "Lambda name is not from top context")
        unless (bExpr == tExpr) (throwError "Top expression is not same as the body of the abstraction")
        unless (TMono bTo == tType) (throwError "Invalid return type")
        unless (TMono bFrom == snd nameAndType) (throwError "Invalid type: lambda type is not context type")
        return True
  _                                                             -> throwError "Incorrect rule for this term or Type of abstraction is not in form σ → τ"

checkRule LetRule  { bottom, topLeft, topRight } = case bottom of
  Typing bCntx (Let bVar bLet bIn) bType@(TMono _) ->
    case (topLeft, topRight) of
      (Typing tlCntx tlExpr tlType, Typing trCntx trExpr trType@(TMono _)) -> do
        unless (bCntx == tlCntx) (throwError "Contexts of binding part are not same")
        unless (extendContext bCntx (bVar, tlType) == trCntx) (throwError "Variable is not added to ctx with correct type")
        unless (bIn == trExpr) (throwError "Terms of 'in' expression are not same")
        unless (bLet == tlExpr) (throwError "Terms of the binding part are not same")
        unless (bType == trType) (throwError "Types of 'in' expression are not same")
        return True
  _                                      -> throwError "Incorrect rule for this term"

checkRule InstRule { bottom, top } = case bottom of
  Typing bCntx bExpr bType ->
    case top of
      Typing tCntx tExpr tType -> do
        unless (bCntx == tCntx) (throwError "InstRule -> bCntx == tCntx")
        unless (bExpr == tExpr) (throwError "InstRule -> bExpr == tExpr")
        unless (runIdentity $ isSpecializedType tType bType) (throwError "Type2 is not a subtype")
        return True

checkRule GenRule  { bottom, top } = case bottom of
  Typing bCntx bExpr (Forall bVar bType) -> do
    case top of
      Typing tCntx tExpr tType -> do
        unless (bCntx == tCntx) (throwError "GenRule -> bCntx == tCntx")
        unless (bExpr == tExpr) (throwError "GenRule -> bExpr == tExpr")

        let bVarName = head bVar
        when (memberTypeContext bCntx (TMono . TVar $ bVarName)) (throwError "Quantified type variable is in free variables of context")
        let bType' = if length bVar == 1 then TMono bType else Forall (tail bVar) bType
        unless (bType' == tType) (throwError "Type3 is incorrectly generalized")
        return True
  _                                      -> throwError "Incorrect rule for this term"


f [] = []
f (' ':a) = f a
f (a:b) = a : f b

ruleNumberToRule :: Int -> [Typing] -> Rule
ruleNumberToRule 1 [bottom]                    = VarRule bottom
ruleNumberToRule 2 [bottom, topLeft, topRight] = AppRule bottom topLeft topRight
ruleNumberToRule 3 [bottom, top]               = AbsRule bottom top
ruleNumberToRule 4 [bottom, topLeft, topRight] = LetRule bottom topLeft topRight
ruleNumberToRule 5 [bottom, top]               = InstRule bottom top
ruleNumberToRule 6 [bottom, top]               = GenRule bottom top
ruleNumberToRule _ _                           = undefined

