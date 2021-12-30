module HW2.T4
  ( -- * The State section
  State (..)
  , joinState
  , mapState
  , modifyState
  , wrapState

  -- * The Prim&Expr section
  , Prim (..)
  , Expr (..)
  , eval
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Control.Monad

import HW2.T1 (Annotated (..), mapAnnotated)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

data State s a = S { runS :: s -> Annotated s a }

data Prim a
  = Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum

data Expr
  = Val Double
  | Op (Prim Expr)

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState $ fmap f m

instance Num Expr where
  x + y = Op $ Add x y
  x * y = Op $ Mul x y
  abs x = Op $ Abs x
  signum x = Op $ Sgn x
  (-) x y = Op $ Sub x y
  fromInteger x = Val $ fromInteger x

instance Fractional Expr where
  fromRational x = Val $ fromRational x
  (/) x y = Op $ Div x y

----------------------------------------------------------------------
-- State internal utilities
----------------------------------------------------------------------

mapState :: (a -> b) -> State s a -> State s b
mapState f (S run) = S $ \x -> mapAnnotated f $ run x

wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

joinState :: State s (State s a) -> State s a
joinState outer = S $ \s ->
  let (inner :# s') = runS outer s
  in runS inner s'

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

----------------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------------

eval :: Expr -> State [Prim Double] Double
eval (Val n)        = return n
eval (Op (Abs x))   = executeUnaryOp (abs) (Abs) x
eval (Op (Sgn x))   = executeUnaryOp (signum) (Sgn) x
eval (Op (Add x y)) = executeBinaryOp (+) (Add) x y
eval (Op (Sub x y)) = executeBinaryOp (-) (Sub) x y
eval (Op (Mul x y)) = executeBinaryOp (*) (Mul) x y
eval (Op (Div x y)) = executeBinaryOp (/) (Div) x y

executeUnaryOp
  :: (Double -> Double)          -- ^ Unary operation
  -> (Double -> Prim Double)     -- ^ Type constructor for unary operation
  -> Expr                        -- ^ Argument
  -> State [Prim Double] Double  -- ^ Evaluation result
executeUnaryOp op prim arg = do
  x <- eval arg
  modifyState ((prim x) :)
  return $ op x

executeBinaryOp
  :: (Double -> Double -> Double)       -- ^ Binary operation
  -> (Double -> Double -> Prim Double)  -- ^ Type constructor for binary operation
  -> Expr                               -- ^ First argument
  -> Expr                               -- ^ Second argument
  -> State [Prim Double] Double         -- ^ Evaluation result
executeBinaryOp op prim x y = do
  x' <- eval x
  y' <- eval y
  modifyState ((prim x' y') :)
  return $ x' `op` y'
