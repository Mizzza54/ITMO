module HW2.T5
  ( ExceptState (..)
  , EvaluationError (..)
  , eval
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , throwExceptState
  , wrapExceptState
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Control.Monad

import HW2.T1 (Annotated (..), Except (..), mapAnnotated, mapExcept)
import HW2.T4 (Expr (..), Prim (..))

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

data EvaluationError = DivideByZero

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

----------------------------------------------------------------------
-- ExceptState internal utilities
----------------------------------------------------------------------

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES run) = ES $ \x -> mapExcept (mapAnnotated f) (run x)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState outer = ES $ \s ->
  let except = runES outer s
  in case except of
    Success a -> let (inner :# s') = a in runES inner s'
    Error e   -> Error e

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

----------------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------------

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val n) = return n
eval (Op (Abs x)) = executeUnaryOp (abs) (Abs) x
eval (Op (Sgn x)) = executeUnaryOp (signum) (Sgn) x
eval (Op (Add x y)) = executeBinaryOp (+) (Add) x y
eval (Op (Sub x y)) = executeBinaryOp (-) (Sub) x y
eval (Op (Mul x y)) = executeBinaryOp (*) (Mul) x y
eval (Op (Div x y)) = do
  x' <- eval x
  y' <- eval y
  if y' == 0
    then do
      throwExceptState DivideByZero
    else do
      modifyExceptState ((Div  x' y') :)
      return (x' / y')

executeUnaryOp
  :: (Double -> Double)                                -- ^ Unary operation
  -> (Double -> Prim Double)                           -- ^ Type constructor for unary operation
  -> Expr                                              -- ^ Argument
  -> ExceptState EvaluationError [Prim Double] Double  -- ^ Evaluation result
executeUnaryOp op prim arg = do
  x <- eval arg
  modifyExceptState ((prim x) :)
  return $ op x

executeBinaryOp
  :: (Double -> Double -> Double)                      -- ^ Binary operation
  -> (Double -> Double -> Prim Double)                 -- ^ Type constructor for binary operation
  -> Expr                                              -- ^ First argument
  -> Expr                                              -- ^ Second argument
  -> ExceptState EvaluationError [Prim Double] Double  -- ^ Evaluation result
executeBinaryOp op prim x y = do
  x' <- eval x
  y' <- eval y
  modifyExceptState ((prim x' y') :)
  return $ x' `op` y'
