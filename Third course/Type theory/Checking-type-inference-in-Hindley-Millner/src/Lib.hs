module Lib
  ( isCorrectProofTree
  ) where

import           Control.Monad.Except (Except)
import           ProofTree            (ProofTree (..))
import           Rule                 (checkRule, ruleNumberToRule)

isCorrectProofTree :: ProofTree -> Except String Bool
isCorrectProofTree (Node typing ruleNumber leaves) = do
  let tops = map (\(Node t _ _) -> t) leaves
  let rule = ruleNumberToRule ruleNumber (typing : tops)
  result <- checkRule rule
  if result
    then do
      results <- traverse isCorrectProofTree leaves
      return $ and results
    else
      return False
