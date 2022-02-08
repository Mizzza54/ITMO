{-# LANGUAGE NoImplicitPrelude #-}

module HW2.T2
  ( distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Data.Monoid (Monoid, mempty, (<>))
import Data.Semigroup (Semigroup)

import HW2.T1

----------------------------------------------------------------------
-- Implementation of distF
----------------------------------------------------------------------

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption (_, _)           = None

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair ((P a b), (P c d)) = P (a, c) (b, d)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad ((Q a1 b1 c1 d1), (Q a2 b2 c2 d2)) = Q (a1, a2) (b1, b2) (c1, c2) (d1, d2)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated ((a1 :# e1), (a2 :# e2)) = (a1, a2) :# (e1 <> e2)

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e
distExcept (Success a, Success b) = Success (a, b)

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, High b)     = High (a, b)

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (h1 :> t1, h2 :> t2) = (h1, h2) :> distStream (t1, t2)

distList :: (List a, List b) -> List (a, b)
distList (_, Nil)         = Nil
distList (Nil, _)         = Nil
distList (a@(_ :. al), b) = concatList (helper a b) (distList (al, b))

helper :: List a -> List b -> List (a, b)
helper _ Nil                  = Nil
helper Nil _                  = Nil
helper a@(ah :. _) (bh :. bl) = (ah, bh) :. (helper a bl)

concatList :: List a -> List a -> List a
concatList a Nil        = a
concatList Nil a        = a
concatList (ah :. al) b = ah :. concatList al b

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\x -> (f x, g x))

----------------------------------------------------------------------
-- Implementation of wrapF
----------------------------------------------------------------------

wrapOption :: a -> Option a
wrapOption a = Some a

wrapPair :: a -> Pair a
wrapPair a = P a a

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept :: a -> Except e a
wrapExcept a = Success a

wrapPrioritised :: a -> Prioritised a
wrapPrioritised a = Low a

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

wrapList :: a -> List a
wrapList a = a :. Nil

wrapFun :: a -> Fun i a
wrapFun a = F (\_ -> a)
