module HW1.T4
  ( tfoldr
  , treeToList
  ) where

import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ acc Leaf             = acc
tfoldr f acc (Branch _ l v r) = tfoldr f (f v (tfoldr f acc r)) l

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []
