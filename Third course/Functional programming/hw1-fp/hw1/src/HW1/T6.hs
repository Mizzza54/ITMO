module HW1.T6
  ( epart
  , mcat
  ) where

mcat :: Monoid a => [Maybe a] -> a
mcat = foldl append mempty 
  where
    append :: Semigroup p => p -> Maybe p -> p
    append acc (Just x) = acc <> x
    append acc Nothing  = acc

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldl append (mempty, mempty) 
  where
    append :: (Semigroup a, Semigroup b) => (a, b) -> Either a b -> (a, b)
    append acc (Left a)  = (fst acc <> a, snd acc)
    append acc (Right b) = (fst acc, snd acc <> b)
