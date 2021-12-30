{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DerivingStrategies #-}

module Parser
  ( Tree (..)
  , runParse
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Applicative ((<|>))
import           Lexer               (Token (..), runTokenize)
import           Utils               (Except (..), ExceptState (..), evaluate,
                                      pEof, run, satisfyPredicate, pOk)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

data Tree = Node String [Tree] | Leaf Token deriving (Show, Eq)

type Parser a = ExceptState [Token] a

----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

assert :: Token -> Parser Token
assert t = satisfyPredicate (== t)

isWord :: Token -> Bool
isWord t = case t of
  WORD _ -> True
  _ -> False

----------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------

parseD :: Parser Tree
parseD = do
  var <- Leaf <$> assert VAR
  name <- Leaf <$> satisfyPredicate isWord
  colon <- Leaf <$> assert COLON
  array <- Leaf <$> assert ARRAY
  open_chevrons <- Leaf <$> assert OPEN_CHEVRONS
  arrayType <- parseType
  close_chevrons <- Leaf <$> assert CLOSE_CHEVRONS
  semicolon <- Leaf <$> assert SEMICOLON
  return $ Node "D" [var, name, colon, array, open_chevrons, arrayType, close_chevrons, semicolon]

--parseType :: Parser Tree
--parseType = (Leaf <$> satisfyPredicate isWord) <|> do
--  array <- Leaf <$> assert ARRAY
--  open_chevrons <- Leaf <$> assert OPEN_CHEVRONS
--  arrayType <- parseType
--  close_chevrons <- Leaf <$> assert CLOSE_CHEVRONS
--  return $ Node [array, open_chevrons, arrayType, close_chevrons]
  
parseType :: ExceptState [Token] Tree
parseType = do
  word <- Leaf <$> (satisfyPredicate isWord <|> assert ARRAY)
  type2 <- parseType2
  return $ Node "T" [word, type2]

parseType2 :: ExceptState [Token] Tree
parseType2 = do
  open_chevrons <- Leaf <$> assert OPEN_CHEVRONS
  type1 <- parseType
  close_chevrons <- Leaf <$> assert CLOSE_CHEVRONS
  comma <- Leaf <$> assert COMMA
  type2 <- parseType
  return $ Node "T2" [open_chevrons, type1 , close_chevrons, comma, type2]
  <|> do
  open_chevrons <- Leaf <$> assert OPEN_CHEVRONS
  type' <- parseType
  close_chevrons <- Leaf <$> assert CLOSE_CHEVRONS
  return $ Node "T2" [open_chevrons, type' , close_chevrons]
  <|> do
  comma <- Leaf <$> assert COMMA
  type' <- parseType
  return $ Node "T2" [comma, type']
  <|> do
  pOk
  return $ Node "T2" [Leaf EPS]

runParse :: String -> Except String Tree
runParse s = let
  parser = parseD <* pEof
  tokens = runTokenize s in
    case tokens of
      Error e -> Error e
      Success r -> case evaluate r $ run parser r of
        Error e    -> Error e
        Success r' -> Success r'
