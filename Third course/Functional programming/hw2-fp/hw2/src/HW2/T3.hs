{-# LANGUAGE NoImplicitPrelude #-}

module HW2.T3
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Data.Semigroup (Semigroup, (<>))

import HW2.T1

----------------------------------------------------------------------
-- Implementation of joinF
----------------------------------------------------------------------

joinOption :: Option (Option a) -> Option a
joinOption (Some (Some a)) = Some a
joinOption _               = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)             = Error e
joinExcept (Success (Error e))   = Error e
joinExcept (Success (Success a)) = Success a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e2) :# e1) = a :# (e1 <> e2)

joinList :: List (List a) -> List a
joinList Nil                        = Nil
joinList (Nil :. a)                 = joinList a
joinList (a :. Nil)                 = a
joinList ((h1 :. t1) :. (h2 :. t2)) = concatList (h1 :. (concatList t1 h2)) (joinList t2)

concatList :: List a -> List a -> List a
concatList a Nil        = a
concatList Nil a        = a
concatList (ah :. al) b = ah :. concatList al b

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\x -> apply (f x) x)
  where
    apply :: Fun i a -> i -> a
    apply (F f') i = f' i
