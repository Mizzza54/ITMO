module Main where

import           Control.Monad.Except (runExcept)
import           Lib                  (isCorrectProofTree)
import           Parser
import           ProofTree            (makeProofTree)

main :: IO ()
main = do
  contents <- getContents
  isCorrect contents

mainFromFile :: IO ()
mainFromFile = do
  contents <- readFile "input.in"
  isCorrect contents

isCorrect :: String -> IO ()
isCorrect contents = case parse contents of
  Failed _ -> putStrLn "Incorrect"
  Ok r     -> case makeProofTree r of
    Left _     -> putStrLn "Incorrect"
    Right tree -> case runExcept $ isCorrectProofTree tree of
      Left e     -> putStrLn "Incorrect"
      Right bool -> if bool then putStrLn "Correct" else putStrLn "Incorrect"
