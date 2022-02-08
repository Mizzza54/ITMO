{

module Parser where

import Lexer
import Grammar

}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
    '('                                           { TokenOpenBracket }
    ')'                                           { TokenCloseBracket }
    lambda                                        { TokenLambda }
    '.'                                           { TokenDot }
    var                                           { TokenVariable $$ }
%%

Expression       : Application lambda var '.' Expression             { Application $1 (Lambda $3 $5) }
                 | lambda var '.' Expression                         { Lambda $2 $4 }
                 | Application                                       { $1 }

Application      : Application Atom                                  { Application $1 $2 }
                 | Atom                                              { $1 }

Atom             : '(' Expression ')'                                { $2 }
                 | var                                               { Var $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parse :: String -> Expression
parse s = parseTokens $ alexScanTokens s

}