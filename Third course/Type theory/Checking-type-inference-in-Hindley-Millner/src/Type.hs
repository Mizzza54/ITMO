module Type
  ( MonoType (..)
  , PolyType (..)
  , Substitution
  , Substitutable (..)
  , Typing (..)
  , Context (..)
  , extendContext
  , memberContext
  , memberTypeContext
  , memberNameContext
  , differenceContext
  , sizeContext
  , getFirstContext
  , emptySubstitution
  , compositionSubstitution
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Set   (Set)
import qualified Data.Set   as Set
import           Expression (Expression (..), VarName)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

type Substitution = Map VarName MonoType

data MonoType
  = TVar VarName
  | TArrow MonoType MonoType
  deriving (Show, Eq, Ord)

data PolyType
  = TMono MonoType
  | Forall [VarName] MonoType
  deriving (Show, Ord)

newtype Context = Context (Map VarName PolyType) deriving (Show, Eq, Ord)

data Typing = Typing Context Expression PolyType deriving (Show, Eq, Ord)

instance Eq PolyType where
  TMono type1 == TMono type2               = type1 == type2
  Forall list1 type1 == Forall list2 type2 = type1 == type2 && (Set.fromList list1 == Set.fromList list2)
  _ == _                                   = False

----------------------------------------------------------------------
-- Classes
----------------------------------------------------------------------

class Substitutable a where
  apply :: Substitution -> a -> a
  ftv   :: a -> Set VarName
  btv   :: a -> Set VarName

instance Substitutable MonoType where
  apply s t@(TVar a)       = Map.findWithDefault t a s
  apply s (l `TArrow` r) = apply s l `TArrow` apply s r

  ftv (TVar a)         = Set.singleton a
  ftv (l `TArrow` r) = ftv l `Set.union` ftv r

  btv _ = Set.empty

instance Substitutable PolyType where
  apply s (Forall as t) = Forall as $ apply (foldr Map.delete s as) t
  apply s (TMono t)     = TMono $ apply s t

  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as
  ftv (TMono t)     = ftv t

  btv (Forall as _) = Set.fromList as
  btv (TMono _)     = Set.empty

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty
  btv   = foldr (Set.union . btv) Set.empty

instance Substitutable Context where
  apply s (Context env) =  Context $ Map.map (apply s) env
  ftv (Context env) = ftv $ Map.elems env
  btv (Context env) = btv $ Map.elems env

----------------------------------------------------------------------
-- Some utils
----------------------------------------------------------------------

extendContext :: Context -> (VarName, PolyType) -> Context
extendContext (Context context) (x, s) = Context $ Map.insert x s context

memberContext :: Context -> Expression -> PolyType -> Bool
memberContext (Context context) (Var name) type' = case Map.lookup name context of
  Just a  -> a == type'
  Nothing -> False
memberContext _ _ _                              = False

memberTypeContext :: Context -> PolyType -> Bool
memberTypeContext (Context context) type' = Set.member type' (Set.fromList (Map.elems context))

memberNameContext :: Context -> Expression -> Bool
memberNameContext (Context context) (Var name) = Map.member name context
memberNameContext _ _                          = False

differenceContext :: Context -> Context -> Context
differenceContext (Context cntx1) (Context cntx2) = Context $ Map.difference cntx1 cntx2

sizeContext :: Context -> Int
sizeContext (Context context) = Map.size context

getFirstContext :: Context -> (VarName, PolyType)
getFirstContext (Context context) = Map.elemAt 0 context

emptySubstitution :: Substitution
emptySubstitution = Map.empty

compositionSubstitution :: Substitution -> Substitution -> Substitution
compositionSubstitution s1 s2 = Map.map (apply s1) s2 `Map.union` s1
