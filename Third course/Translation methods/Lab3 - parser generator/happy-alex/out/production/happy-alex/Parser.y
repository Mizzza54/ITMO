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
    word   { TokenChar $$ }
%%

Expressions        : Expression Expressions               { $1 : $2 }
                   | {- -}                                { [] }

Expression         : '$' FormulaList '$'                  { ExpressionFormula $2 }

FormulaList        : Formula FormulaList                  { $1 : $2 }
                   | {- -}                                { [] }

Formula            : Data SubAndSup                       { Formula $1 (fst $2) (snd $2)}

Data               : SimpleWord                           { SimpleWord $1 }
                   | Complex                              { $1 }

SimpleWord         : word SimpleWord                      { $1 ++ $2 }
                   | {- -}                                { "" }

Complex            : '\\' word CommandArgs                { Complex $2 $3 }

CommandArgs        : {- empty -}                          { [] }
                   | '\{' Expression '\}' CommandArgs     { $2 : $4 }

SubAndSup          : Sub Sup                              { ($1, $2) }
                   | Sup Sub                              { ($2, $1) }
                   | Sub                                  { ($1, Nothing) }
                   | Sup                                  { (Nothing, $1) }
                   | {- -}                                { (Nothing, Nothing) }

Sub                : '_' Data                             { Just $2 }

Sup                : '^' Data                             { Just $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: String -> Expressions
parse s = Expressions (parseTokens $ alexScanTokens s)

}