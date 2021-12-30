module Tests
  ( tests
  ) where

import           Hedgehog           (Gen)
import           Lexer              (Token (..), runTokenize)
import           Parser             (runParse)
import           Test.Hspec         (anyErrorCall, it, shouldBe, shouldNotBe,
                                     shouldThrow)
import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.Hspec   (it, shouldBe, testSpec)

import qualified FailLexerTests     as FLT
import qualified FailParserTests    as FPT
import qualified SuccessLexerTests  as SLT
import qualified SuccessParserTests as SPT
import qualified SuccessCommaParserTests as SCPT

successTestLexer :: IO TestTree
successTestLexer = testSpec "Success Lexer tests:" $ do
  it (show SLT.lexerTest1) $ runTokenize SLT.lexerTest1 `shouldBe` SLT.expectedLexerTest1
  it (show SLT.lexerTest2) $ runTokenize SLT.lexerTest2 `shouldBe` SLT.expectedLexerTest2
  it (show SLT.lexerTest3) $ runTokenize SLT.lexerTest3 `shouldBe` SLT.expectedLexerTest3
  it (show SLT.lexerTest4) $ runTokenize SLT.lexerTest4 `shouldBe` SLT.expectedLexerTest4
  it (show SLT.lexerTest5) $ runTokenize SLT.lexerTest5 `shouldBe` SLT.expectedLexerTest5
  it (show SLT.lexerTest6) $ runTokenize SLT.lexerTest6 `shouldBe` SLT.expectedLexerTest6
  it (show SLT.lexerTest7) $ runTokenize SLT.lexerTest7 `shouldBe` SLT.expectedLexerTest7
  it (show SLT.lexerTest8) $ runTokenize SLT.lexerTest8 `shouldBe` SLT.expectedLexerTest8
  it (show SLT.lexerTest9) $ runTokenize SLT.lexerTest9 `shouldBe` SLT.expectedLexerTest9

successTestParser :: IO TestTree
successTestParser = testSpec "Success Parser tests:" $ do
  it (show SPT.parserTest1) $ runParse SPT.parserTest1 `shouldBe` SPT.expectedParserTest1
  it (show SPT.parserTest2) $ runParse SPT.parserTest2 `shouldBe` SPT.expectedParserTest2
  it (show SPT.parserTest3) $ runParse SPT.parserTest3 `shouldBe` SPT.expectedParserTest3

failTestLexer :: IO TestTree
failTestLexer = testSpec "Fail Lexer tests:" $ do
  it (show FLT.lexerTest1) $ runTokenize FLT.lexerTest1 `shouldBe` FLT.expectedLexerTest

failTestParser :: IO TestTree
failTestParser = testSpec "Fail Parser tests:" $ do
  it (show FPT.parserTest1) $ runParse FPT.parserTest1 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest2) $ runParse FPT.parserTest2 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest3) $ runParse FPT.parserTest3 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest4) $ runParse FPT.parserTest4 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest5) $ runParse FPT.parserTest5 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest6) $ runParse FPT.parserTest6 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest7) $ runParse FPT.parserTest7 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest8) $ runParse FPT.parserTest8 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest9) $ runParse FPT.parserTest9 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest10) $ runParse FPT.parserTest10 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest11) $ runParse FPT.parserTest11 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest12) $ runParse FPT.parserTest12 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest13) $ runParse FPT.parserTest13 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest14) $ runParse FPT.parserTest14 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest15) $ runParse FPT.parserTest15 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest16) $ runParse FPT.parserTest16 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest17) $ runParse FPT.parserTest17 `shouldBe` FPT.expectedParserTest
  it (show FPT.parserTest18) $ runParse FPT.parserTest18 `shouldBe` FPT.expectedParserTest

successCommaTestParser :: IO TestTree
successCommaTestParser = testSpec "Success CommaParser tests:" $ do
  it (show SCPT.parserTest1) $ runParse SCPT.parserTest1 `shouldBe` SCPT.expectedParserTest1
  it (show SCPT.parserTest2) $ runParse SCPT.parserTest2 `shouldBe` SCPT.expectedParserTest2
  it (show SCPT.parserTest3) $ runParse SCPT.parserTest3 `shouldBe` SCPT.expectedParserTest3

tests :: IO TestTree
tests = do
  successLexer <- successTestLexer
  successParser <- successTestParser
  failLexer<- failTestLexer
  failParser <- failTestParser
  successCommaParser <- successCommaTestParser
  return $ testGroup "All tests:" [successLexer, successParser, failLexer, failParser, successCommaParser]
