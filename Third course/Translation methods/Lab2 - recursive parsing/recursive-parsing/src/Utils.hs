{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}

module Utils
  ( -- * The Except section
    Except (..)

    -- * The Annotated section
  , Annotated (..)

    -- * The ExceptState section
  , ExceptState (..)
  , ErrorAtPos (..)
  , joinExceptState
  , throwExceptState
  , modifyExceptState
  , run
  , next
  , evaluate
  , incState
  , satisfyPredicate
  , space
  , skipSpaces
  , pEof
  , getPos
  , pOk
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Applicative (Alternative, empty, many, some, (<|>))
import           Control.Monad       (MonadPlus, ap, mfilter, void)

import           Data.Char           (isAlpha, isSpace)

import           GHC.Natural         (Natural, naturalToInt, naturalToInteger)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

data Except e a
  = Error e
  | Success a
  deriving (Show)

data Annotated e a = a :# e deriving (Show)
infix 0 :#

newtype ErrorAtPos = ErrorAtPos Natural deriving Show

type State s = (Natural, s)

newtype ExceptState s a = ES {runES :: State s -> Except ErrorAtPos (Annotated (State s) a)}

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance (Eq a) => Eq (Except e a) where
  Success a == Success b = a == b
  (Error _) == (Error _) = True
  _ == _                 = False

instance Functor (Except e) where
  fmap _ (Error e)   = Error e
  fmap f (Success a) = Success (f a)

instance Functor (Annotated e) where
  fmap f (a :# e) = f a :# e

instance Functor (ExceptState s) where
  fmap f (ES run) = ES (fmap (fmap f) . run)

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

----------------------------------------------------------------------
-- Simple methods
----------------------------------------------------------------------

run :: ExceptState s a -> s -> Except ErrorAtPos a
run inner t = case runES inner (0, t) of
  Success (a :# _) -> Success a
  Error e          -> Error e

next :: ExceptState [a] a
next = ES \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

getPos :: ExceptState [a] Natural
getPos = ES \(pos, s) -> Success (pos :# (pos, s))

evaluate :: (Show s) => [s] -> Except ErrorAtPos a -> Except String a
evaluate s a = case a of
  Error (ErrorAtPos e) -> Error $ "Error at pos "
                                ++ show e
                                ++ "\n"
                                ++ show s
                                ++ "\n"
                                ++ replicate (helper s (naturalToInt e)) ' '
                                ++ "^"
  Success r            -> Success r

helper :: (Show s) => [s] -> Int -> Int
helper s n = foldl (\acc i -> acc + (length . show) i) 0 (take n s) + 1

incState :: State a -> State a
incState s = (fst s + 1, snd s)

----------------------------------------------------------------------
-- Simple parsers
----------------------------------------------------------------------

satisfyPredicate :: (a -> Bool) -> ExceptState [a] a
satisfyPredicate p = mfilter p next

space :: ExceptState String ()
space = void $ some $ satisfyPredicate isSpace

skipSpaces :: ExceptState String a -> ExceptState String a
skipSpaces p = do
  void $ many space
  res <- p
  void $ many space
  pure res

pEof :: ExceptState [a] ()
pEof = ES \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, s))
    _  -> Error (ErrorAtPos pos)

pOk :: ExceptState b ()
pOk = ES \(pos, s) -> Success (() :# (pos, s))