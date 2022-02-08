{

module Parser where

import qualified Data.Map   as Map
import           Expression (Expression (..))
import           Lexer
import           ProofTree  (Line (..))
import           Type       (Context (..), MonoType (..), PolyType (..))

}

%name parseTokens
%tokentype { Token }
%monad { E } { thenE } { returnE }
%error { parseError }

%token
    '('                                           { TokenOpenBracket }
    ')'                                           { TokenCloseBracket }
    lambda                                        { TokenLambda }
    '.'                                           { TokenDot }
    var                                           { TokenVariable $$ }
    let                                           { TokenLet }
    in                                            { TokenIn }
    rule                                          { TokenRule $$ }
    forall                                        { TokenForall }
    ':'                                           { TokenColon }
    offset                                        { TokenOffset }
    ','                                           { TokenComma }
    '|-'                                          { TokenTurnstile }
    '->'                                          { TokenArrow }
    '='                                           { TokenEqual }
%%

Proof               : Line Proof                                    { $1 : $2 }
                    |                                               { [] }

Offset              : offset Offset                                 { 1 + $2 }
                    |                                               { 0 }

Line                : Offset Context '|-' ExpressionWithType rule   { Line $1 $2 (fst $4) (snd $4) $5 }

Context             : var ':' Type ',' Context                      { ($1, $3) : $5 }
                    | var ':' Type                                  { [($1, $3)] }
                    |                                               { [] }

ExpressionWithType  : Expression ':' Type                           { ($1, $3) }

Type                : '(' Type ')'                                  { $2 }
                    | MonoType                                      { TMono $1 }
                    | forall var '.' ManyForall                     { Forall ($2 : fst $4) (snd $4) }

ManyForall          : forall var '.' ManyForall                     { ($2 : fst $4, snd $4) }
                    | MonoType                                      { ([], $1) }

MonoType            : var                                           { TVar $1 }
                    | '(' MonoType ')' '->' MonoType                { TArrow $2 $5 }
                    | var '->' MonoType                             { TArrow (TVar $1) $3 }
                    | '(' MonoType ')'                              { $2 }

Expression          : Application lambda var '.' Expression         { Application $1 (Lambda $3 $5) }
                    | lambda var '.' Expression                     { Lambda $2 $4 }
                    | let var '=' Expression in Expression          { Let $2 $4 $6 }
                    | Application                                   { $1 }

Application         : Atom                                          { $1 }
                    | Application Atom                              { Application $1 $2 }

Atom                : '(' Expression ')'                            { $2 }
                    | var                                           { Var $1 }


{

parseError :: [Token] -> E a
parseError tokens = failE "Parse error"

parse :: String -> E [Line]
parse s = parseTokens $ alexScanTokens s

data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = case m of
  Ok a     -> k a
  Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = case m of
  Ok a     -> Ok a
  Failed e -> k e

}