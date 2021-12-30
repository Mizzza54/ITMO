{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
module Utils.Base
  ( Parser
  , ExceptState (..)
  , Except (..)
  , Annotated (..)
  , ParseError (..)
  , modifyExceptState
  , throwExceptState
  , run
  , evaluate
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Monad
import           GHC.Natural         (Natural)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

data Annotated e a = a :# e
infix 0 :#

data Except e a
  = Error e
  | Success a
  deriving (Show)

newtype ExceptState s a = ES {runES :: State s -> Except ParseError (Annotated (State s) a)}

type State s = (Natural, s)

newtype ParseError = ErrorAtPos Natural deriving (Show)

type Parser s a = ExceptState s a

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Functor (Annotated e) where
  fmap f (a :# e) = f a :# e

instance Functor (Except e) where
  fmap _ (Error e)   = Error e
  fmap f (Success a) = Success (f a)

instance (Monoid e) => Applicative (Annotated e) where
  pure a = a :# mempty
  (f :# e1) <*> (x :# e2) = f x :# (e1 <> e2)

instance Applicative (Except e) where
  pure a = Success a
  (Error e) <*> _             = Error e
  _ <*> (Error e)             = Error e
  (Success f) <*> (Success x) = Success $ f x

instance Functor (ExceptState s) where
  fmap f (ES run) = ES $ \x -> fmap (fmap f) (run x)

instance Applicative (ExceptState s) where
  pure a = ES $ \s -> Success (a :# s)
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState s) where
  m >>= f = joinExceptState (fmap f m)

instance Alternative (ExceptState s) where
  empty = ES \(pos, _) -> Error (ErrorAtPos pos)

  (<|>) es1 es2 = ES \(pos, s) ->
    case runES es1 (pos, s) of
      Success (a :# s') -> Success (a :# s')
      Error _ -> case runES es2 (pos, s) of
        Success (b :# s') -> Success (b :# s')
        Error e           -> Error e

instance MonadPlus (ExceptState s)   -- No methods.

----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

joinExceptState :: ExceptState s (ExceptState s a) -> ExceptState s a
joinExceptState outer = ES $ \s ->
  let except = runES outer s
  in case except of
    Success a -> let (inner :# s') = a in runES inner s'
    Error e   -> Error e

modifyExceptState :: (State s -> State s) -> ExceptState s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: Natural -> ExceptState s a
throwExceptState e = ES $ \_ -> Error (ErrorAtPos e)

run :: ExceptState s a -> s -> Except ParseError a
run inner t = case runES inner (0, t) of
  Success (a :# _) -> Success a
  Error e          -> Error e
  
evaluate :: Except ParseError a -> Except String a
evaluate a = case a of
  Error (ErrorAtPos e) -> Error $ show e
  Success r            -> Success r