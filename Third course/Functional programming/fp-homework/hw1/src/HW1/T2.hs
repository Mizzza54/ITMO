module HW1.T2
  ( N (..)
  , nEven
  , nFromNatural
  , nOdd
  , nToNum
  , ncmp
  , ndiv
  , nmod
  , nmult
  , nplus
  , nsub
  ) where

import GHC.Natural (Natural)

data N
  = Z
  | S N
  deriving (Show)

nplus :: N -> N -> N
nplus Z n            = n
nplus n Z            = n
nplus n'@(S _) (S m) = S (nplus n' m)

nmult :: N -> N -> N
nmult Z _            = Z
nmult _ Z            = Z
nmult (S Z) n        = n
nmult n (S Z)        = n
nmult (S n) m'@(S _) = nplus m' (nmult n m')

nsub :: N -> N -> Maybe N
nsub Z Z         = Just Z
nsub Z _         = Nothing
nsub n Z         = Just n
nsub (S n) (S m) = nsub n m

ncmp :: N -> N -> Ordering
ncmp n m = case nsub n m of
  Just Z  -> EQ
  Nothing -> LT
  _       -> GT

nFromNatural :: Natural -> N
nFromNatural n = nFromNatural' n Z
  where
    nFromNatural' :: (Eq t, Num t) => t -> N -> N
    nFromNatural' 0 acc  = acc
    nFromNatural' n' acc = nFromNatural' (n' - 1) (S acc)

nToNum :: Num a => N -> a
nToNum n = nToNum' n 0
  where
    nToNum' :: Num t => N -> t -> t
    nToNum' Z acc     = acc
    nToNum' (S m) acc = nToNum' m (1 + acc)

nEven, nOdd :: N -> Bool
nEven Z         = True
nEven (S Z)     = False
nEven (S (S n)) = nEven n
nOdd            = not . nEven

ndiv :: N -> N -> N
ndiv n m = fst $ pairDivMod n m Z

nmod :: N -> N -> N
nmod n m = snd $ pairDivMod n m Z

pairDivMod :: N -> N -> N -> (N, N)
pairDivMod n m acc
  | ncmp n m == LT = (acc, n)
  | otherwise      = case nsub n m of
    Just r -> pairDivMod r m (S acc)
    _      -> pairDivMod Z m (S acc)
