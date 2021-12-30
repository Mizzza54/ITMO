module Lib where

import Data.List (intercalate)
import HW1.T3

data ParentDir = PLeft | PRight | NoParent deriving (Show,Eq)
type ParentPos = Int
type Level = Int

dline :: Char
dline = '|'
factor :: Int
factor = 4

m :: Char -> Char -> Char
m c1 c2 = if c1 == dline then c1 else c2
zipWith' :: (t -> t -> t) -> [t] -> [t] -> [t]
zipWith' _ xs [] = xs
zipWith' _ [] xs = xs
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

build_line :: Show a => ParentDir -> a -> [Int] -> Int -> [Char]
build_line pd a pp level = foldl (zipWith' m) "" (((++"|").(flip replicate ' ') <$> (factor*) <$> pp)++[(replicate (factor*level) ' ')++cn++show a])
                           where cn = case pd of PLeft -> "└──"
                                                 PRight -> "┌──"
                                                 NoParent -> "───"

tprint :: Show a => ParentDir -> [ParentPos] -> Level -> Tree a -> [String]
tprint _ _ _ Leaf = []
tprint pd pp level (Branch _ l a r) = tprint PRight new_pp_r (level+1) r ++
                                  [build_line pd a pp level] ++
                                  tprint PLeft new_pp_l (level+1) l
                                  where new_pp_r = case pd of PRight -> pp
                                                              PLeft -> pp++[level]
                                                              NoParent -> pp
                                        new_pp_l = case pd of PRight -> pp++[level]
                                                              PLeft -> pp
                                                              NoParent -> pp

printt :: Show a => Tree a -> IO ()
printt t = putStr $ (intercalate "\r\n" (tprint NoParent [] 0 t))++"\r\n"