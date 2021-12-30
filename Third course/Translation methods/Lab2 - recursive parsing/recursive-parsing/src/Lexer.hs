{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}

module Lexer
  ( -- * The Lexical section
    Lexical
  , Token (..)
  , runTokenize
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Applicative (empty, some, (<|>))
import           Control.Monad       (void)

import           Data.Char           (isAlphaNum)

import           Utils               (Except (..), ExceptState (..), evaluate,
                                      pEof, run, satisfyPredicate, skipSpaces,
                                      space)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

type Lexical a = ExceptState String a

data Token
  = VAR
  | WORD String
  | ARRAY
  | OPEN_CHEVRONS
  | CLOSE_CHEVRONS
  | SEMICOLON
  | COLON
  | COMMA
  | EPS

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Eq Token where
  VAR == VAR                       = True
  (WORD x) == (WORD y)             = x == y
  ARRAY == ARRAY                   = True
  OPEN_CHEVRONS == OPEN_CHEVRONS   = True
  CLOSE_CHEVRONS == CLOSE_CHEVRONS = True
  SEMICOLON == SEMICOLON           = True
  COLON == COLON                   = True
  COMMA == COMMA                   = True
  EPS == EPS                       = True
  _ == _                           = False

instance Show Token where
  show VAR            = "VAR"
  show (WORD a)       = "WORD " ++ "\\\"" ++ a ++ "\\\""
  show ARRAY          = "ARRAY"
  show OPEN_CHEVRONS  = "OPEN_CHEVRONS"
  show CLOSE_CHEVRONS = "CLOSE_CHEVRONS"
  show SEMICOLON      = "SEMICOLON"
  show COLON          = "COLON"
  show COMMA          = "COMMA"
  show EPS            = "EPS"

----------------------------------------------------------------------
--  Simple parsers
----------------------------------------------------------------------

satisfyString :: String -> Lexical String
satisfyString [] = empty
satisfyString [c] = (:[]) <$> satisfyPredicate (== c)
satisfyString (c:cs) = do
  void $ satisfyPredicate (== c)
  satisfyString cs

----------------------------------------------------------------------
--  Tokens parsers
----------------------------------------------------------------------

isVar :: Lexical Token
isVar = satisfyString "var" >> (space <|> pEof) >> pure VAR

isWord :: Lexical Token
isWord = do
  r <- some $ satisfyPredicate predicate
  pure $ WORD r
    where
      predicate c = isAlphaNum c || c == '_'

isArray :: Lexical Token
isArray = satisfyString "Array" >> pure ARRAY

isOpenChevrons :: Lexical Token
isOpenChevrons = satisfyPredicate (== '<') >> pure OPEN_CHEVRONS

isCloseChevrons :: Lexical Token
isCloseChevrons = satisfyPredicate (== '>') >> pure CLOSE_CHEVRONS

isSemicolon :: Lexical Token
isSemicolon = satisfyPredicate (== ';') >> pure SEMICOLON

isColon :: Lexical Token
isColon = satisfyPredicate (== ':') >> pure COLON

isComma :: Lexical Token
isComma = satisfyPredicate (== ',') >> pure COMMA

isToken :: Lexical Token
isToken = skipSpaces $ isVar <|> isColon <|> isArray <|> isOpenChevrons <|> isCloseChevrons <|> isSemicolon <|> isWord <|> isComma

tokenize :: Lexical [Token]
tokenize = do
  r <- isToken
  e <- tokenize
  pure (r : e)
  <|> pure []

runTokenize :: String -> Except String [Token]
runTokenize s = evaluate s $ run lexical s where
  lexical = tokenize <* pEof
