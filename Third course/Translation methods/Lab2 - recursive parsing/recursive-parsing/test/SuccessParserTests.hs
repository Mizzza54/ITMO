module SuccessParserTests where

import           Lexer  (Token (..))
import           Parser (Tree (..))
import           Utils  (Except (..))

parserTest1 :: String
parserTest1 = "var x: Array<Int>;"

expectedParserTest1 :: Except String Tree
expectedParserTest1 = Success $ 
  Node "D" [Leaf VAR, Leaf (WORD "x"), Leaf COLON, Leaf ARRAY, Leaf OPEN_CHEVRONS, 
    Node "T" [Leaf (WORD "Int"), Node "T2" [Leaf EPS]], 
  Leaf CLOSE_CHEVRONS, Leaf SEMICOLON]

parserTest2 :: String
parserTest2 = "var   x   :   Array   <Int>   ;"

expectedParserTest2 :: Except String Tree
expectedParserTest2 = expectedParserTest1

parserTest3 :: String
parserTest3 = "var name: Array<Array<Array<Type>>>;"

expectedParserTest3 :: Except String Tree
expectedParserTest3 = Success $
  Node "D" [Leaf VAR, Leaf (WORD "name"), Leaf COLON, Leaf ARRAY, Leaf OPEN_CHEVRONS, 
    Node "T" [Leaf ARRAY, 
      Node "T2" [Leaf OPEN_CHEVRONS,
        Node "T" [Leaf ARRAY,
          Node "T2" [Leaf OPEN_CHEVRONS,
            Node "T" [Leaf (WORD "Type"), 
              Node "T2" [Leaf EPS]],
            Leaf CLOSE_CHEVRONS]],
          Leaf CLOSE_CHEVRONS]],
        Leaf CLOSE_CHEVRONS,Leaf SEMICOLON]