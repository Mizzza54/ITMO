{

module Lexer where

}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-z]

tokens :-
    $white                                ;
    \(                                    { \s -> TokenOpenBracket }
    \)                                    { \s -> TokenCloseBracket }
    \\                                    { \s -> TokenLambda }
    "."                                   { \s -> TokenDot }
    $alpha ($alpha | $digit | \')*        { \s -> TokenVariable s }

{

data Token
  = TokenOpenBracket
  | TokenCloseBracket
  | TokenLambda
  | TokenDot
  | TokenVariable String
  deriving (Eq, Show)

}