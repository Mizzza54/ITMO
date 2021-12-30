module Syntax
  ( Expressions (..)
  , Expression (..)
  , Formula (..)
  , Data (..)
  ) where


newtype Expressions = Expressions [Expression]

newtype Expression = ExpressionFormula [Formula]

data Formula = Formula Data (Maybe Data) (Maybe Data)

data Data
  = SimpleChar String
  | SimpleWord String
  | Complex String [Expression]

instance Show Expressions where
  show (Expressions array) = unlines $ map show array

instance Show Expression where
  show (ExpressionFormula array) = "<math>\n" ++ showFormulas array ++ "</math>" where
    showFormulas = foldl join ""
    join b a = b ++ show a ++ "\n"

instance Show Formula where
  show (Formula body Nothing Nothing)
    =  "<mo>"
    ++ show body
    ++ "</mo>"
  show (Formula body Nothing (Just sup))
    =  "<msup>"
    ++ "<mo>"
    ++ show body
    ++ "</mo>"
    ++ "<mn>"
    ++ show sup
    ++ "</mn>"
    ++ "</msup>"
  show (Formula body (Just sub) Nothing)
    =  "<msub>"
    ++ "<mo>"
    ++ show body
    ++ "</mo>"
    ++ "<mn>"
    ++ show sub
    ++ "</mn>"
    ++ "</msub>"
  show (Formula body (Just sub) (Just sup))
    =  "<msubsup>"
    ++ "<mo>"
    ++ show body
    ++ "</mo>"
    ++ "<mn>"
    ++ show sub
    ++ "</mn>"
    ++ "<mi>"
    ++ show sup
    ++ "</mi>"
    ++ "</msubsup>"

instance Show Data where
  show (SimpleChar ch)     = ch
  show (SimpleWord word)   = word
  show (Complex str exprs) = "Complex ????"
