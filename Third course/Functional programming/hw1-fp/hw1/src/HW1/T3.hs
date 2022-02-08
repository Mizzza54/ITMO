module HW1.T3
  ( Tree (..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList,
  ) where

import Data.Foldable (foldl')

data SizeAndDepth = SD
  { size  :: Int  -- ^ Size
  , depth :: Int  -- ^ Depth
  } deriving (Show)

data Tree a
  = Leaf
  | Branch SizeAndDepth (Tree a) a (Tree a)
  deriving (Show)

-- | Get left subtree.
tl :: Tree a -> Tree a
tl Leaf                = Leaf
tl (Branch _ left _ _) = left

-- | Get right subtree.
tr :: Tree a -> Tree a
tr Leaf                 = Leaf
tr (Branch _ _ _ right) = right

-- | Get value of node.
tv :: Tree a -> a
tv Leaf                 = error "Can not get value from leaf."
tv (Branch _ _ value _) = value

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf              = 0
tsize (Branch sd _ _ _) = size sd

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf              = 0
tdepth (Branch sd _ _ _) = depth sd

-- | Check if the element is in the tree, O(LOG N)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember key (Branch _ l v r)
  | key == v  = True
  | key < v   = tmember key l
  | otherwise = tmember key r

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert key Leaf = mkBranch Leaf key Leaf
tinsert key tree@(Branch _ l v r)
  | key < v   = rotate $ mkBranch (tinsert key l) v r
  | key > v   = rotate $ mkBranch l v (tinsert key r)
  | otherwise = tree

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = foldl' (flip tinsert) Leaf

-- | Check balance of node
isBalancedDepth :: Tree a -> Bool
isBalancedDepth Leaf = True
isBalancedDepth (Branch _ left _ right)
  | not (isBalancedDepth left)           = False
  | not (isBalancedDepth right)          = False
  | abs (tdepth left - tdepth right) > 1 = False
  | otherwise                            = True

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l v r = Branch sd l v r
  where
    sd = SD (tsize l + tsize r + 1) (max (tdepth $ l) (tdepth $ r) + 1)

rotate :: Tree a -> Tree a
rotate Leaf = Leaf
rotate tree@(Branch _ l v r)
  | not (isBalancedDepth l)                                  = mkBranch (rotate l) v r
  | not (isBalancedDepth r)                                  = mkBranch l v (rotate r)
  | tdepth l + 1 < tdepth r && tdepth (tl r) < tdepth (tr r) = sLR tree
  | tdepth r + 1 < tdepth l && tdepth (tr l) < tdepth (tl l) = sRR tree
  | tdepth l + 1 < tdepth r && tdepth (tl r) > tdepth (tr r) = bLR tree
  | tdepth r + 1 < tdepth l && tdepth (tr l) > tdepth (tl l) = bRR tree
  | otherwise                                                = tree

-- | Small left rotate.
sLR :: Tree a -> Tree a
sLR Leaf             = Leaf
sLR (Branch _ l v r) = mkBranch (mkBranch l v $ tl r) (tv r) (tr r)

-- | Small right rotate.
sRR :: Tree a -> Tree a
sRR Leaf             = Leaf
sRR (Branch _ l v r) = mkBranch (tl l) (tv l) (mkBranch (tr l) v r)

-- | Big left rotate.
bLR :: Tree a -> Tree a
bLR Leaf             = Leaf
bLR (Branch _ l v r) = sLR (mkBranch l v (sRR r))

-- | Big right rotate.
bRR :: Tree a -> Tree a
bRR Leaf             = Leaf
bRR (Branch _ l v r) = sRR (mkBranch (sLR l) v r)
