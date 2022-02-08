module HW1.T7
  ( -- * The ListPlus section
  ListPlus (..)

  -- * The Inclusive section
  , Inclusive (..)

   -- * The DotString section
  , DotString (..)

   -- * The Fun section
  , Fun (..)
  ) where

data ListPlus a
  = a :+ ListPlus a
  | Last a

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last x) (Last y)   = x :+ Last y
  (<>) (x :+ xs) (Last y)  = x :+ (xs <> Last y)
  (<>) (Last x) (y :+ ys)  = x :+ (y :+ ys)
  (<>) (x :+ xs) (y :+ ys) = x :+ (xs <> (y :+ ys))

data Inclusive a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This a) (This b)     = This (a <> b)
  (<>) (This a) (That b)     = Both a b
  (<>) (This a) (Both b c)   = Both (a <> b) c
  (<>) (That a) (This b)     = Both b a
  (<>) (That a) (That b)     = That (a <> b)
  (<>) (That a) (Both b c)   = Both b (a <> c)
  (<>) (Both a b) (This c)   = Both (a <> c) b
  (<>) (Both a b) (That c)   = Both a (b <> c)
  (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)

newtype DotString = DS String deriving (Show)

instance Semigroup DotString where
  (<>) (DS x) (DS "") = DS x
  (<>) (DS "") (DS x) = DS x
  (<>) (DS x) (DS y)  = DS (x ++ "." ++ y)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F a) (F b) = F (a . b)

instance Monoid (Fun a) where
  mempty = F id
