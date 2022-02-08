module Lib where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Monad.State
import           Data.IORef          (newIORef, readIORef, writeIORef)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           GHC.IO.Unsafe       (unsafePerformIO)
import           Grammar

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

type VariableName = String

type IndexFreeVar = Int

type IsNormalForm = Bool

type FreeVars = Set String

type Replace = Map String String

type ExprBVars = Set String

type Environment = (IndexFreeVar, IsNormalForm, FreeVars)

----------------------------------------------------------------------
-- Environment Functions
----------------------------------------------------------------------

incrementIndexFreeVar :: State Environment ()
incrementIndexFreeVar = modify (\(a, b, c) -> (a + 1, b, c))

setNormalFormFlag :: IsNormalForm -> State Environment ()
setNormalFormFlag bool = modify (\(a, _, c) -> (a, bool, c))

getIndexFreeVar :: State Environment IndexFreeVar
getIndexFreeVar = state (\(a, b, c) -> (a, (a, b, c)))

getNormalFormFlag ::  State Environment IsNormalForm
getNormalFormFlag = state (\(a, b, c) -> (b, (a, b, c)))

addFreeVar :: String -> State Environment ()
addFreeVar var = modify (\(a, b, c) -> (a, b, Set.insert var c))

getFreeVars :: State Environment FreeVars
getFreeVars = state (\(a, b, c) -> (c, (a, b, c)))

fv :: Expression -> FreeVars
fv (Var x)                   = Set.singleton x
fv (Application exprA exprB) = fv exprA `Set.union` fv exprB
fv (Lambda var expr)         = Set.delete var $ fv expr
fv (Memorization ref)        = fv $ unsafePerformIO $ readIORef ref

isVar :: Expression -> Bool
isVar (Var _)            = True
isVar (Memorization ref) = case unsafePerformIO $ readIORef ref of
                           Var _ -> True
                           _     -> False
isVar _                  = False

isApplication :: Expression -> Bool
isApplication (Application _ _) = True
isApplication _                 = False

templateFreeVar :: Int -> String
templateFreeVar n = "v" ++ show n

substitution :: Expression -> VariableName -> Expression -> ExprBVars -> Replace -> State Environment Expression
substitution exprA@(Var var) varName exprB exprBVars replace = do
  let guard' | Map.member var replace = return (Var ((Map.!) replace var))
             | var == varName         = return $ Memorization $ unsafePerformIO $ newIORef exprB
             | otherwise              = return exprA
  guard'
substitution (Lambda var lambdaExpr) varName expr exprBVars replace = do
  freeVars <- getFreeVars
  let guard' | Set.member var exprBVars = do
              index <- getIndexFreeVar
              incrementIndexFreeVar
              let newVar = templateFreeVar index
              result <- substitution lambdaExpr varName expr exprBVars (Map.insert var newVar replace)
              return $ Lambda newVar result
            | var == varName = do
              return $ Lambda var lambdaExpr
            | otherwise = do
              result <- substitution lambdaExpr varName expr exprBVars replace
              return $ Lambda var result
  guard'
substitution (Application expr1 expr2) varName expr exprBVars replace = do
  result1 <- substitution expr1 varName expr exprBVars replace
  result2 <- substitution expr2 varName expr exprBVars replace
  return $ Application result1 result2
substitution (Memorization ref) varName expr exprBVars replace = do
  let refExpr = unsafePerformIO $ readIORef ref
  substitution refExpr varName expr exprBVars replace

betaReduction :: Expression -> State Environment Expression
betaReduction (Var x) = do
  return $ Var x
betaReduction (Lambda var expr) = do
  expr' <- betaReduction expr
  return $ Lambda var expr'
betaReduction (Application (Lambda var exprA) exprB) = do
  addFreeVar var
  result <- substitution exprA var exprB (Set.insert var (fv exprB)) Map.empty
  setNormalFormFlag False
  return result
betaReduction (Application (Memorization ref) expr) = do
  let refExpr = unsafePerformIO $ readIORef ref
  betaReduction (Application refExpr expr)
betaReduction (Application exprA exprB) = do
  exprA' <- betaReduction exprA
  flag <- getNormalFormFlag
  if not flag then return $ Application exprA' exprB else do
    exprB' <- betaReduction exprB
    return $ Application exprA exprB'
betaReduction (Memorization ref) = do
  expr <- betaReduction . unsafePerformIO $ readIORef ref
  let _ = writeIORef ref expr
  return expr

loop :: Int -> [Expression] -> State Environment [Expression]
loop 0 exprs = return exprs
loop m exprs = do
  result <- betaReduction (head exprs)
  flag <- getNormalFormFlag
  if flag
    then do
      return exprs
    else do
      setNormalFormFlag True
      loop (m - 1) (result : exprs)

eachK :: Int -> [a] -> [a]
eachK k = map snd . filter ((== 1) . fst) . zip (cycle [1..k])

runNormalization :: Int -> Int -> Expression -> [Expression]
runNormalization m k expr = let result =  evalState (loop m [expr]) (0, True, fv expr) in (eachK k . reverse . tail $ result) ++ [head result]
