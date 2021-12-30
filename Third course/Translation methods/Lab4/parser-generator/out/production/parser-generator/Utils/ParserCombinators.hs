{-# LANGUAGE BlockArguments #-}

module Utils.ParserCombinators
  ( pcOk
  , pcFail
  , pcSatisfyPredicate
  , pcNext
  , pcWord
  , pcSpace
  , pcSkipSpaces
  , pcEof
  , pcSatisfyElem
  , pcSatisfyElems
  , pcSkipChar
  , pcBetweenChar
  , pcTakeWhile
  , toRegex
  , pcRegExp
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Applicative (empty, many, some, (<|>))
import           Control.Monad       (mfilter, void)
import           Data.Char           (isSpace)
import           Text.Regex.TDFA
import           Utils.Base          (Annotated (..), Except (..),
                                      ExceptState (..), ParseError (..), Parser)

----------------------------------------------------------------------
-- Simple parsers combinators
----------------------------------------------------------------------

pcOk :: Parser s ()
pcOk = ES \(pos, s) -> Success (() :# (pos, s))

pcFail :: Parser s ()
pcFail = Control.Applicative.empty

pcSatisfyPredicate :: (a -> Bool) -> Parser [a] a
pcSatisfyPredicate p = mfilter p pcNext

pcNext :: ExceptState [a] a
pcNext = ES \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

pcWord :: ExceptState String String
pcWord = many (pcSatisfyPredicate (not . isSpace))

pcSpace :: Parser String ()
pcSpace = void $ some $ pcSatisfyPredicate isSpace

pcSkipSpaces :: Parser String a -> Parser String a
pcSkipSpaces p = do
  void $ many pcSpace
  res <- p
  void $ many pcSpace
  pure res

pcEof :: Parser [a] ()
pcEof = ES \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, s))
    _  -> Error (ErrorAtPos pos)

pcSatisfyElem :: (Eq a) => a -> Parser [a] a
pcSatisfyElem e = pcSatisfyPredicate (== e)

pcSatisfyElems :: (Eq a) => [a] -> Parser [a] [a]
pcSatisfyElems [] = Control.Applicative.empty
pcSatisfyElems [c] = (:[]) <$> pcSatisfyElem c
pcSatisfyElems (c:cs) = do
  void $ pcSatisfyElem c
  pcSatisfyElems cs

pcSkipChar :: Char -> Parser String ()
pcSkipChar ch = void $ pcSkipSpaces $ pcSatisfyElem ch

pcBetweenChar :: Char -> Char -> Parser String b -> Parser String b
pcBetweenChar open close parser = do
  pcSkipChar open
  res <- parser
  pcSkipChar close
  return res

pcTakeWhile :: (a -> Bool) -> ExceptState [a] [a]
pcTakeWhile predicat = many $ pcSatisfyPredicate predicat

toRegex :: String -> Regex
toRegex = makeRegexOpts defaultCompOpt {multiline = False} defaultExecOpt

pcRegExp :: String -> ExceptState String String
pcRegExp regexp = ES \(pos, s) ->
  case matchM (toRegex regexp) s :: Maybe (String, String, String) of
    Nothing         -> Error (ErrorAtPos pos)
    Just (_, x, xs) -> Success (x :# (pos + 1, xs))
    
    
