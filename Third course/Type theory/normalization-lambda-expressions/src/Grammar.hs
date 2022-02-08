{-# LANGUAGE FlexibleInstances #-}

module Grammar where

import           Data.IORef    (IORef, readIORef)
import           GHC.IO.Unsafe (unsafePerformIO)


data Expression
  = Application Expression Expression
  | Lambda String Expression
  | Var String
  | Memorization (IORef Expression)

instance Show (IORef Expression) where
  show x = show $ unsafePerformIO $ readIORef x

instance Show Expression where
  show (Application a b)  = "(" ++ show a ++ " " ++ show b ++ ")"
  show (Lambda a b)       = "(\\" ++ a ++ "." ++ show b ++ ")"
  show (Var var)          = var
  show (Memorization ref) = show ref