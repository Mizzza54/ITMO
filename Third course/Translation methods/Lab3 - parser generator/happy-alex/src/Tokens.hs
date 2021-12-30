module Tokens ( Token (..)) where

data Token
  = TokenBackslash
  | TokenOpenCurlyBrace
  | TokenCloseCurlyBrace
  | TokenDollar
  | TokenCircumflexus
  | TokenUnderline
  | TokenWord String
  | TokenEOF
  | TokenWhiteSpace
  deriving (Eq, Show)