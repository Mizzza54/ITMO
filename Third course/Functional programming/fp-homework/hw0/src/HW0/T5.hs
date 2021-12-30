module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import GHC.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz = \_ x -> x

ns :: Nat a -> Nat a
ns n f x = n f $ f x

nplus :: Nat a -> Nat a -> Nat a
nplus m n f x = m f $ n f x

nmult :: Nat a -> Nat a -> Nat a
nmult a b = a . nplus b nz

nFromNatural :: Natural -> Nat a
nFromNatural 0 = \_ x -> x
nFromNatural n = \f x -> nFromNatural (n - 1) f (f x)

nToNum :: Num a => Nat a -> a
nToNum n = n (+ 1) 0
