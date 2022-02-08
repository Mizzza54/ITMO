{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->) (..)
  , assocEither
  , assocLeftEither
  , assocLeftPair
  , assocPair
  , assocRightEither
  , assocRightPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a)       = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso assocLeftPair assocRightPair

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso assocLeftEither assocRightEither

assocLeftPair :: (a, (b, c)) -> ((a, b), c)
assocLeftPair (a, (b, c)) = ((a, b), c)

assocRightPair :: ((a, b), c) -> (a, (b, c))
assocRightPair ((a, b), c) = (a, (b, c))

assocLeftEither :: Either a (Either b c) -> Either (Either a b) c
assocLeftEither (Left a)          = Left (Left a)
assocLeftEither (Right (Left b))  = Left (Right b)
assocLeftEither (Right (Right c)) = Right c

assocRightEither :: Either (Either a b) c -> Either a (Either b c)
assocRightEither (Left (Left a))  = Left a
assocRightEither (Left (Right b)) = Right (Left b)
assocRightEither (Right c)        = Right (Right c)
