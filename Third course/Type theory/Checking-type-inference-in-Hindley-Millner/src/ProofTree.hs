module ProofTree
  ( ProofTree (..)
  , Line (..)
  , makeProofTree
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Monad.Except (ExceptT, runExceptT, throwError, unless, when)
import           Control.Monad.State  (State, evalState, gets, modify, get, runState)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Expression           (Expression)
import           Type                 (Context (..), PolyType, Typing (..))

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

type RuleNumber = Int

type Offset = Int

data Line = Line Offset [(String, PolyType)] Expression PolyType RuleNumber deriving (Show, Eq, Ord)

data ProofTree = Node Typing RuleNumber [ProofTree] deriving (Show)

----------------------------------------------------------------------
-- Make ProofTree
----------------------------------------------------------------------

--evalState (runExceptT $ processUnaryRule (-1)) lines
makeProofTree :: [Line] -> Either String ProofTree
makeProofTree lines = case runState (runExceptT $ processUnaryRule (-1)) lines of
  (a, []) -> a
  (_, x)  -> Left "bad proof"
  
processUnaryRule :: Offset -> ExceptT String (State [Line]) ProofTree
processUnaryRule predOffset = do
  lines' <- get
  when (null lines') (throwError "Invalid rule")
  line <- gets head
  modify tail
  case line of
    Line offset rawContext expr type' rule -> do
      case mkMapContext rawContext Map.empty of
        Nothing -> throwError "Invalid Context"
        Just map' -> do
          let context = Context map'
          unless (predOffset + 1 == offset) (throwError "Invalid offset")
          let typing = Typing context expr type'
          leaves <- makeLeaves rule offset
          return $ Node typing rule leaves

processBinaryRule :: Offset -> ExceptT String (State [Line]) [ProofTree]
processBinaryRule predOffset = do
  lines1 <- get
  when (null lines1) (throwError "Invalid rule")
  line1 <- gets head
  modify tail
  case line1 of
    Line offset1 rawContext1 expr1 type1 rule1 -> do
      case mkMapContext rawContext1 Map.empty of
        Nothing -> throwError "Invalid Context"
        Just map1 -> do
          let context1 = Context map1
          unless (predOffset + 1 == offset1) (throwError "Invalid offset")
          let typing1 = Typing context1 expr1 type1
          leaves1 <- makeLeaves rule1 offset1
          lines2 <- get
          when (null lines2) (throwError "Invalid rule")
          line2 <- gets head
          modify tail
          case line2 of
            Line offset2 rawContext2 expr2 type2 rule2 -> do
              case mkMapContext rawContext2 Map.empty of
                Nothing -> throwError "Invalid Context"
                Just map2 -> do
                  let context2 = Context map2
                  unless (offset2 == offset1) (throwError "Invalid offset")
                  let typing2 = Typing context2 expr2 type2
                  leaves2 <- makeLeaves rule2 offset2
                  return [Node typing1 rule1 leaves1, Node typing2 rule2 leaves2]

makeLeaves :: RuleNumber -> Offset -> ExceptT String (State [Line]) [ProofTree]
makeLeaves rule offset = case rule of
  1 -> return []
  2 -> processBinaryRule offset
  3 -> (:[]) <$> processUnaryRule offset
  4 -> processBinaryRule offset
  5 -> (:[]) <$> processUnaryRule offset
  6 -> (:[]) <$> processUnaryRule offset
  _ -> throwError "Unexpected number of rule"

mkMapContext :: [(String, PolyType)] -> Map String PolyType -> Maybe (Map String PolyType)
mkMapContext [] map'       = Just map'
mkMapContext (x : xs) map' = case Map.lookup (fst x) map' of
  Just _  -> Nothing
  Nothing -> mkMapContext xs (uncurry Map.insert x map')