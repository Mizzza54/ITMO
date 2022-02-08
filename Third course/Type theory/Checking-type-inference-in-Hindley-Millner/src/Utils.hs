module Utils
  ( isSpecializedType
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Monad.Except   (Except, runExceptT, throwError)
import           Control.Monad.Identity (Identity)
import qualified Data.Map               as Map
import qualified Data.Set               as Set
import           Expression
import           Type                   (MonoType (..), PolyType (..),
                                         Substitutable (..), Substitution,
                                         compositionSubstitution,
                                         emptySubstitution)

import Parser
import ProofTree

----------------------------------------------------------------------
-- Utils for Unification & Specialized
----------------------------------------------------------------------

unification ::  MonoType -> MonoType -> Except String Substitution
unification (l `TArrow` r) (l' `TArrow` r')  = do
    s1 <- unification l l'
    s2 <- unification (apply s1 r) (apply s1 r')
    return (s2 `compositionSubstitution` s1)
unification (TVar a) t = bind a t
unification t (TVar a) = bind a t

bind ::  VarName -> MonoType -> Except String Substitution
bind a t | t == TVar a     = return emptySubstitution
         | occursCheck a t = throwError "Occurs check"
         | otherwise       = return $ Map.singleton a t

occursCheck ::  Substitutable a => VarName -> a -> Bool
occursCheck a t = a `Set.member` ftv t

degeneralize :: PolyType -> MonoType
degeneralize (Forall _ t) = t
degeneralize (TMono t)    = t

isSpecializedType :: PolyType -> PolyType -> Identity Bool
isSpecializedType tGen tSpec = do
  let degeneralizedTGen = degeneralize tGen
  let degeneralizedTSpec = degeneralize tSpec
  substitution <- runExceptT $ unification degeneralizedTGen degeneralizedTSpec
  case substitution of
    Left _ -> return False
    Right s -> do
      let list = Map.toList s
      let handler = (\(var, _) -> Set.member var (ftv degeneralizedTGen) && Set.member var (btv tGen))
      return $ foldl (\acc a -> acc && handler a) True list
