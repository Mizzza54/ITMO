module SuccessCommaParserTests where

import           Lexer  (Token (..))
import           Parser (Tree (..))
import           Utils  (Except (..))

parserTest1 :: String
parserTest1 = "var x: Array<Int, Double, Array>;"

expectedParserTest1 :: Except String Tree
expectedParserTest1 = Success $ 
  Node "D" [Leaf VAR, Leaf (WORD "x"), Leaf COLON, Leaf ARRAY, Leaf OPEN_CHEVRONS,
    Node "T" [Leaf (WORD "Int"), 
      Node "T2" [Leaf COMMA, 
        Node "T" [Leaf (WORD "Double"), 
          Node "T2" [Leaf COMMA, 
            Node "T" [Leaf ARRAY, 
              Node "T2" [Leaf EPS]]]]]],
  Leaf CLOSE_CHEVRONS, Leaf SEMICOLON]

parserTest2 :: String
parserTest2 = "var x: Array<Int, Array<Type1, Type2>, Map<Char, String, Boolean>, Integer, Byte>;"

expectedParserTest2 :: Except String Tree
expectedParserTest2 = Success $
  Node "D" [Leaf VAR, Leaf (WORD "x"), Leaf COLON, Leaf ARRAY, Leaf OPEN_CHEVRONS,
    Node "T" [Leaf (WORD "Int"), 
      Node "T2" [Leaf COMMA,
        Node "T" [Leaf ARRAY,
          Node "T2" [Leaf OPEN_CHEVRONS,
            Node "T" [Leaf (WORD "Type1"),
              Node "T2" [Leaf COMMA,
                Node "T" [Leaf (WORD "Type2"),
                  Node "T2" [Leaf EPS]]]],
            Leaf CLOSE_CHEVRONS, Leaf COMMA,
              Node "T" [Leaf (WORD "Map"), 
                Node "T2" [Leaf OPEN_CHEVRONS,
                  Node "T" [Leaf (WORD "Char"),
                    Node "T2" [Leaf COMMA,
                      Node "T" [Leaf (WORD "String"),
                        Node "T2" [Leaf COMMA,
                          Node "T" [Leaf (WORD "Boolean"),
                    Node "T2" [Leaf EPS]]]]]], Leaf CLOSE_CHEVRONS, Leaf COMMA,
                      Node "T" [Leaf (WORD "Integer"), 
                        Node "T2" [Leaf COMMA,
                          Node "T" [Leaf (WORD "Byte"),
                            Node "T2" [Leaf EPS]]]]]]]]]],
    Leaf CLOSE_CHEVRONS,Leaf SEMICOLON]

parserTest3 :: String
parserTest3 = "var name: Array<type1, type2, type1>;"

expectedParserTest3 :: Except String Tree
expectedParserTest3 = Success $
  Node "D" [Leaf VAR, Leaf (WORD "name"), Leaf COLON, Leaf ARRAY, Leaf OPEN_CHEVRONS,
    Node "T" [Leaf (WORD "type1"),
      Node "T2" [Leaf COMMA,
        Node "T" [Leaf (WORD "type2"),
          Node "T2" [Leaf COMMA,
            Node "T" [Leaf (WORD "type1"),
              Node "T2" [Leaf EPS]]]]]],
  Leaf CLOSE_CHEVRONS,Leaf SEMICOLON]