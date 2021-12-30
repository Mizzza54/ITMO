{-# LANGUAGE RankNTypes #-}

module TT where

import Data.Void (Void (..))
  
fix :: forall a . ((a -> a) -> a)
fix f = let x = f x in x
  
controversy :: forall a . a
controversy = fix id
  
type Not a  = a -> Void
type Disjunction a b = forall c . ((a -> c) -> (b -> c) -> c)
type LEM    = forall a.   Either a (Not a)
--type LEM a = Disjunction a (Not a)
type Pierce = forall a b. ((a -> b) -> a) -> a

peirce :: forall a. forall b.  ((a -> b) -> a) -> a
peirce f = let 
  x :: a -> b
  x a = controversy
  in f x

proof :: Pierce -> LEM
proof law = law $ \k -> Right (k . Left)
-- Left                   :: a -> a ∨ b
-- Right                  :: b -> a ∨ b
-- k                      :: a ∨ b -> c
-- (k . Left)             :: a -> c
-- \k. (k . Left)         :: (a ∨ b -> c) -> a -> 
-- Right (k . Left)       :: b ∨ (a -> c)
-- \k -> Right (k . Left) :: (a ∨ b -> c) -> d ∨ (a -> c)
-- peirce ...             :: a ∨ (a -> c)


lem :: LEM
lem = proof peirce

--left :: a -> Disjunction a b
--left x l _ = l x
--
--right :: b -> Disjunction a b
--right x _ r = r x
--
--proof :: Pierce -> LEM a
--proof law = law $ \k -> right (k . left)
--
--lem :: LEM a
--lem = proof peirce
--
data Bin_list a = Nil | Zero (Bin_list (a, a)) | One a (Bin_list (a, a)) deriving Show

add :: a -> Bin_list a -> Bin_list a
add elem' lst = case lst of
  Nil       -> One elem' Nil
  Zero tl   -> One elem' tl
  One hd tl -> Zero (add (elem', hd) tl)