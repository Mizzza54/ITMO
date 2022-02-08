module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import GHC.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' = fix $ \m f list -> case list of
                          []   -> []
                          x:xs -> f x : m f xs

fib :: Natural -> Natural
fib = fix (\f ac1 ac2 n -> if n == 0
                           then ac1
                           else f ac2 (ac1 + ac2) (n - 1)) 0 1

fac :: Natural -> Natural
fac = fix (\f ac1 n -> if n == 0
                       then ac1
                       else f (ac1 * n) (n - 1)) 1
