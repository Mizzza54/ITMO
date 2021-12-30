module SuccessLexerTests where

import           Lexer (Token (..))
import           Utils (Except (..))

lexerTest1 :: String
lexerTest1 = "var x: Array<Int>;"

expectedLexerTest1 :: Except String [Token]
expectedLexerTest1 = Success [VAR, WORD "x", COLON, ARRAY, OPEN_CHEVRONS, WORD "Int", CLOSE_CHEVRONS, SEMICOLON]

lexerTest2 :: String
lexerTest2 = "var"

expectedLexerTest2 :: Except String [Token]
expectedLexerTest2 = Success [VAR]

lexerTest3 :: String
lexerTest3 = "var var"

expectedLexerTest3 :: Except String [Token]
expectedLexerTest3 = Success [VAR, VAR]

lexerTest4 :: String
lexerTest4 = "varvar"

expectedLexerTest4 :: Except String [Token]
expectedLexerTest4 = Success [WORD "varvar"]

lexerTest5 :: String
lexerTest5 = "var   x   :   Array   <Int>   ;"

expectedLexerTest5 :: Except String [Token]
expectedLexerTest5 = Success [VAR, WORD "x", COLON, ARRAY, OPEN_CHEVRONS, WORD "Int", CLOSE_CHEVRONS, SEMICOLON]

--lexerTest6 :: String
--lexerTest6 = "var Array: Array<Int>;"
--
--expectedLexerTest6 :: Except String [Token]
--expectedLexerTest6 = [VAR, WORD "Array", COLON, ARRAY, OPEN_CHEVRONS, WORD "Int", CLOSE_CHEVRONS, SEMICOLON]

lexerTest6 :: String
lexerTest6 = "  var name: Array<Int>;  "

expectedLexerTest6 :: Except String [Token]
expectedLexerTest6 = Success [VAR, WORD "name", COLON, ARRAY, OPEN_CHEVRONS, WORD "Int", CLOSE_CHEVRONS, SEMICOLON]

lexerTest7 :: String
lexerTest7 = "var\nname\n:\nArray\n<\nInt\n>\n;\n"

expectedLexerTest7 :: Except String [Token]
expectedLexerTest7 = Success [VAR, WORD "name", COLON, ARRAY, OPEN_CHEVRONS, WORD "Int", CLOSE_CHEVRONS, SEMICOLON]

lexerTest8 :: String
lexerTest8 = "var name: Array<Array<Array<Type>>>;"

expectedLexerTest8 :: Except String [Token]
expectedLexerTest8 = Success [VAR, WORD "name", COLON, ARRAY, OPEN_CHEVRONS, ARRAY, OPEN_CHEVRONS, ARRAY, OPEN_CHEVRONS, WORD "Type", CLOSE_CHEVRONS, CLOSE_CHEVRONS, CLOSE_CHEVRONS, SEMICOLON]

lexerTest9 :: String
lexerTest9 = "var name_surname42: Array<Type>;"

expectedLexerTest9 :: Except String [Token]
expectedLexerTest9 = Success [VAR, WORD "name_surname42", COLON, ARRAY, OPEN_CHEVRONS, WORD "Type", CLOSE_CHEVRONS, SEMICOLON]
