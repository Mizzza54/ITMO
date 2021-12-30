module LexerGenerator.Base
  ( LexerFile (..)
  , Rule (..)
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Data.List   (intercalate)
import           Text.Printf (printf)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

data LexerFile = LexerFile
  { haskellCodeHeader :: String
  , lexerName         :: String
  , tokenName         :: String
  , rules             :: [Rule]
  , haskellCodeFooter :: String
  }

data Rule = Rule
  { regex :: String
  , code  :: String
  }

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Show Rule where
  show rule = printf " = (%s) <$> pcRegExp \"^%s\"\n" (code rule) (regex rule)

instance Show LexerFile where
  show lexer = printf
    lexerFileTemplate
      (haskellCodeHeader lexer)
      (intercalate "\n" $ zipWith printImpl [1 :: Int ..] $ rules lexer)
      (intercalate " <|> " (map toTokenParseName [1 .. (length (rules lexer))] ))
      (lexerName lexer)
      (haskellCodeFooter lexer)

----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

toTokenParseName :: Int -> String
toTokenParseName x = "tokenParse" ++ show x

printImpl :: Int -> Rule -> String
printImpl num rule = toTokenParseName num ++ show rule

lexerFileTemplate :: String
lexerFileTemplate =
  "\                                                                                \n\
   \%s                                                                              \n\
   \                                                                                \n\
   \import           Control.Applicative     (many, (<|>))                          \n\
   \import           Utils.Base              (Except (..), ParseError (..), Parser, \n\
   \                                          run)                                  \n\
   \import           Utils.ParserCombinators (pcEof, pcRegExp, pcSkipSpaces)        \n\
   \                                                                                \n\
   \%s                                                                              \n\
   \                                                                                \n\
   \commonLexer = %s                                                                \n\
   \                                                                                \n\
   \%s = many $ pcSkipSpaces commonLexer                                            \n\
   \                                                                                \n\
   \%s                                                                              \n"
