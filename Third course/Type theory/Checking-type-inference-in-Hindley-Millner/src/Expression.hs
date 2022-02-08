module Expression
  ( Expression (..)
  , VarName
  ) where

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

type VarName = String

data Expression
  = Application Expression Expression
  | Lambda VarName Expression
  | Var VarName
  | Let VarName Expression Expression
  deriving (Show, Eq, Ord)
