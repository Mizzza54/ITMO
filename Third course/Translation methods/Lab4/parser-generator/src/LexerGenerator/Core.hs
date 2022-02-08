module LexerGenerator.Core where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Applicative     (many, (<|>))
import           Control.Monad           (void)
import           Data.Char               (isAlphaNum)
import           LexerGenerator.Base
import           Utils.Base              (Except (..), ParseError (..), Parser,
                                          modifyExceptState, run)
import           Utils.ParserCombinators

----------------------------------------------------------------------
-- Internal simple parsers
----------------------------------------------------------------------

parseHaskellCode :: Parser String String
parseHaskellCode = pcBetweenChar '{' '}' (pcTakeWhile (/= '}'))

parseLexerName :: Parser String String
parseLexerName = pcSatisfyElems "%lexername" >> parseHaskellCode

parseTokenName :: Parser String String
parseTokenName = pcSatisfyElems "%tokenname" >> parseHaskellCode

parseRegexPattern :: Parser String String
parseRegexPattern = do
  x <- pcBetweenChar '%' '$' (pcTakeWhile (/= '$'))
  return x

parseRules :: Parser String Rule
parseRules = do
  regex' <- parseRegexPattern <|> pcWord
  void $ many pcSpace
  code' <- parseHaskellCode
  return $ Rule {regex = regex', code = code'}

----------------------------------------------------------------------
-- Main parser
----------------------------------------------------------------------

runParseLexicalFile :: Parser String LexerFile
runParseLexicalFile = do
  header <- parseHaskellCode
  lexerName' <- parseLexerName
  tokenName' <- parseTokenName
  rules' <- many parseRules
  footer <- parseHaskellCode
  return $ LexerFile
    { haskellCodeHeader = header
    , lexerName = lexerName'
    , tokenName = tokenName'
    , rules = rules'
    , haskellCodeFooter = footer
    }
