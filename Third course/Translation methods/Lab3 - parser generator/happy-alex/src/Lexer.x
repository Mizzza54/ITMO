{

module Lexer (
  Token(..),
  alexScanTokens
) where

import Tokens

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$math_symbols = [= \+ \- \* \/]
$eol   = [\n]

tokens :-
  $eol                                     ;
  $white+                                  ;

  \\                                       { \s -> TokenBackslash }
  \{                                       { \s -> TokenOpenCurlyBrace }
  \}                                       { \s -> TokenCloseCurlyBrace }
  \$                                       { \s -> TokenDollar }
  \^                                       { \s -> TokenCircumflexus }
  _                                        { \s -> TokenUnderline }
  ($digit | $alpha | $math_symbols)        { \s -> TokenWord s }
