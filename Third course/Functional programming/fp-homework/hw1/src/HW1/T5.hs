module HW1.T5
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..), (<|))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ []     = [] :| []
splitOn sep list =
  let (h, t) = break (== sep) list
  in if (null (drop 1 t) && length t /= 1)
    then h :| []
    else h <| splitOn sep (drop 1 t)

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep =
  let join = \l r -> l ++ sep : r
  in foldl1 join
