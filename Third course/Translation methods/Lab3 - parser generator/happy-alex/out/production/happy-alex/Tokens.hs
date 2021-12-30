module Tokens 
  ( Token
  , concatTokenChar
  ) where

data Token
  = TokenBackslash
  | TokenOpenCurlyBrace
  | TokenCloseCurlyBrace
  | TokenDollar
  | TokenCircumflexus
  | TokenUnderline
  | TokenChar String
  | TokenEOF
  deriving (Eq,Show)

concatTokenChar (TokenChar a) (TokenChar b) = TokenChar (a ++ b) 