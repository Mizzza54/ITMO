{

module Parser where

import Lexer
import Syntax

}

%name parseTokens
%tokentype { Token }

-- Parser monad
%error { parseError }

-- Token Names
%token
    '\\'   { TokenBackslash }
    '\{'   { TokenOpenCurlyBrace }
    '\}'   { TokenCloseCurlyBrace }
    '$'    { TokenDollar }
    '^'    { TokenCircumflexus }
    '_'    { TokenUnderline }
    word   { TokenWord $$ }
%%

Expressions        : Expression Expressions               { $1 : $2 }
                   | {- -}                                { [] }

Expression         : '$' FormulaList '$'                  { ExpressionFormula $2 }

FormulaList        : Formula FormulaList                  { $1 : $2 }
                   | {- -}                                { [] }

Formula            : Data SubAndSup                       { Formula $1 (fst $2) (snd $2)}

Data               : SimpleWord                           { SimpleWord $1 }

SimpleWord         : word                                 { $1 }

SubAndSup          : Sub Sup                              { ($1, $2) }
                   | Sup Sub                              { ($2, $1) }
                   | Sub                                  { ($1, Nothing) }
                   | Sup                                  { (Nothing, $1) }
                   | {- -}                                { (Nothing, Nothing) }

Sub                : '_' '\{' FormulaList '\}'            { Just (Formulas $3) }
                   | '_' Data                             { Just (Formulas [(Formula $2 Nothing Nothing)]) }

Sup                : '^' '\{' FormulaList '\}'            { Just (Formulas $3) }
                   | '^' Data                             { Just (Formulas [(Formula $2 Nothing Nothing)]) }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: String -> Expressions
parse s = Expressions (parseTokens $ alexScanTokens s)

}