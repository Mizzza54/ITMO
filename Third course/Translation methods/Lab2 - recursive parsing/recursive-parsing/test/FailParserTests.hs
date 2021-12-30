module FailParserTests where

import           Lexer  (Token (..))
import           Parser (Tree (..))
import           Utils  (Except (..))

expectedParserTest :: Except String Tree
expectedParserTest = Error ""

parserTest1 :: String
parserTest1 = "var x: Array<>;"

parserTest2 :: String
parserTest2 = "var x: Array<Array<Type> Type>;"

parserTest3 :: String
parserTest3 = "var : Array<Type>;"

parserTest4 :: String
parserTest4 = "name: Array<Type>;"

parserTest5 :: String
parserTest5 = "var name Array<Type>;"

parserTest6 :: String
parserTest6 = "var name : <Type>;"

parserTest7 :: String
parserTest7 = "var name : Array<Type>"

parserTest8 :: String
parserTest8 = "var name : Array Type;"

parserTest9 :: String
parserTest9 = "var name : Array <Type;"

parserTest10 :: String
parserTest10 = "var name : Array Type>;"

parserTest11 :: String
parserTest11 = "var name : Array<,>;"

parserTest12 :: String
parserTest12 = "var name : Array<Int,>;"

parserTest13 :: String
parserTest13 = "var name : Array<Type1, Type2,>;"

parserTest14 :: String
parserTest14 = "var name : Array<Type1, , Type2>;"

parserTest15 :: String
parserTest15 = "var name : Array<Map<Int, >>;"

parserTest16 :: String
parserTest16 = "var name : Array<Map<,>>;"

parserTest17 :: String
parserTest17 = "var name : Array<Map<>>;"

parserTest18 :: String
parserTest18 = "var name : Array<Int, Map<>>;"