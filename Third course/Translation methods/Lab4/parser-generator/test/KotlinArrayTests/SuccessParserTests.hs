module KotlinArrayTests.SuccessParserTests where

import           GeneratedKotlinArray.Lexer  (Token (..))
import           GeneratedKotlinArray.Parser (Tree (..))
import           Utils.Base  (Except (..), ParseError (..))

parserTest1 :: String
parserTest1 = "var x: Array<Int>;"

expectedParserTest1 :: Except ParseError Tree
expectedParserTest1 = Success $ 
  Node "D" [Leaf VAR, Leaf (WORD "x"), Leaf COLON, Leaf ARRAY, Leaf OPEN_CHEVRONS, 
    Node "T" [Leaf (WORD "Int")], 
  Leaf CLOSE_CHEVRONS, Leaf SEMICOLON]

parserTest2 :: String
parserTest2 = "var   x   :   Array   <Int>   ;"

expectedParserTest2 :: Except ParseError Tree
expectedParserTest2 = expectedParserTest1

parserTest3 :: String
parserTest3 = "var name: Array<Array<Array<Type>>>;"

expectedParserTest3 :: Except ParseError Tree
expectedParserTest3 = Success $
  Node "D" [Leaf VAR, Leaf (WORD "name"), Leaf COLON, Leaf ARRAY, Leaf OPEN_CHEVRONS, 
    Node "T" [Leaf ARRAY, Leaf OPEN_CHEVRONS, 
      Node "T" [Leaf ARRAY, Leaf OPEN_CHEVRONS, Node "T" [Leaf (WORD "Type")], Leaf CLOSE_CHEVRONS], 
    Leaf CLOSE_CHEVRONS],
  Leaf CLOSE_CHEVRONS,Leaf SEMICOLON]