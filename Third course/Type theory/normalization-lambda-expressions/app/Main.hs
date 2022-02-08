module Main where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Monad.Cont            (lift)
import           Control.Monad.State           (evalState)
import qualified Data.Set                      as Set
import           Lib                           (Environment, betaReduction,
                                                runNormalization)
import           Parser                        (parse)

----------------------------------------------------------------------
-- Main
----------------------------------------------------------------------

main :: IO ()
main = do
  numbers <- getLine
  let m = read $ head (words numbers) :: Int
  let k = read $ last (words numbers) :: Int
  expr <- getLine
  traverse print (runNormalization m k (parse expr))
  return ()
