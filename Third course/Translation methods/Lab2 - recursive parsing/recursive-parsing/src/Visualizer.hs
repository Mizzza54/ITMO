{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}

module Visualizer (runVisualizer) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Parser         (Tree (..), runParse)

import           GHC.Natural    (Natural)
import           Lexer          (Token (..))
import           System.Process (callCommand)
import           Utils          (Except (..), ExceptState, evaluate, getPos,
                                 incState, modifyExceptState, run)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

newtype VisualizerError = ErrorAtPos Natural deriving Show

type Visualizer a = ExceptState String a

----------------------------------------------------------------------
-- Visualization
----------------------------------------------------------------------

runVisualizer :: String -> IO ()
runVisualizer str = do
  let tree = runParse str
  let graph = func $ result tree
  writeFile "./out/tree.gv" graph
  callCommand "dot -Tjpg ./out/tree.gv -o ./out/tree.jpg"
  where
    func str' = "graph \"\" {\n" ++ str' ++ "}\n"
    result tree = case tree of
      Error e -> errorWithoutStackTrace e
      Success r -> case evaluate str $ run (mkGraph r) "" of
        Error e'   -> errorWithoutStackTrace e'
        Success r' -> r'

mkGraph :: Tree -> Visualizer String
mkGraph (Leaf x) = do
  num <- getPos
  modifyExceptState (incState . incState)
  return $ "n"
         ++ show num
         ++ " [label = \""
         ++ show x
         ++ "\"];\n"
         ++ "n"
         ++ show (num + 1)
         ++ " [label = \""
         ++ printToken x
         ++ "\"];\n"
         ++ "n"
         ++ show num
         ++ " -- n"
         ++ show (num + 1)
         ++ ";\n"
mkGraph (Node node (x : xs)) = do
  numCur <- getPos
  next <- mkGraph (Node node xs)
  numChild <- getPos
  cur <- mkGraph x
  return $ "n"
         ++ show numCur
         ++ " -- n"
         ++ show numChild
         ++ ";\n"
         ++ cur
         ++ next
mkGraph (Node node []) = do
  num <- getPos
  modifyExceptState incState
  return $ "n"
         ++ show num
         ++ " [label = \"" ++ node ++"\"];\n"

printToken :: Token -> String
printToken token = case token of
  VAR            -> "var"
  WORD x         -> x
  ARRAY          -> "Array"
  OPEN_CHEVRONS  -> "<"
  CLOSE_CHEVRONS -> ">"
  SEMICOLON      -> ";"
  COLON          -> ":"
  EPS            -> ""
  COMMA          -> ","
