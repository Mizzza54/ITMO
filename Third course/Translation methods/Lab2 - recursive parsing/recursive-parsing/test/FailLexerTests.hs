module FailLexerTests where

import           Lexer (Token (..))
import           Utils (Except (..))

expectedLexerTest :: Except String [Token]
expectedLexerTest = Error ""

lexerTest1 :: String
lexerTest1 = "?"
