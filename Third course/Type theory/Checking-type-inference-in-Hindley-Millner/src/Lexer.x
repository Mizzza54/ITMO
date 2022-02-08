{

module Lexer where

}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-z]
$ruleNumber = [1-6]

tokens :-
    $white                                ;
    \(                                    { \s -> TokenOpenBracket }
    \)                                    { \s -> TokenCloseBracket }
    \\                                    { \s -> TokenLambda }
    "."                                   { \s -> TokenDot }
    let                                   { \s -> TokenLet }
    in                                    { \s-> TokenIn }
    "[rule #1]"                           { \s-> TokenRule 1 }
    "[rule #2]"                           { \s-> TokenRule 2 }
    "[rule #3]"                           { \s-> TokenRule 3 }
    "[rule #4]"                           { \s-> TokenRule 4 }
    "[rule #5]"                           { \s-> TokenRule 5 }
    "[rule #6]"                           { \s-> TokenRule 6 }
    forall                                { \s-> TokenForall }
    ":"                                   { \s-> TokenColon }
    "*   "                                { \s-> TokenOffset }
    ","                                   { \s-> TokenComma }
    "|-"                                  { \s-> TokenTurnstile }
    "->"                                  { \s-> TokenArrow }
    "="                                   { \s-> TokenEqual }
    $alpha ($alpha | $digit | \')*        { \s -> TokenVariable s }

{

data Token
  = TokenOpenBracket
  | TokenCloseBracket
  | TokenLambda
  | TokenDot
  | TokenVariable String
  | TokenLet
  | TokenIn
  | TokenRule Int
  | TokenForall
  | TokenColon
  | TokenOffset
  | TokenComma
  | TokenTurnstile
  | TokenArrow
  | TokenEqual
  deriving (Eq, Show)

}