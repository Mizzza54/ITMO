module KotlinArrayTests.Starter where

import           GeneratedKotlinArray.Lexer          (Token (..), tokenize)
import           GeneratedKotlinArray.Parser         (evaluate)
import qualified KotlinArrayTests.SuccessLexerTests  as SLT
import qualified KotlinArrayTests.SuccessParserTests as SPT
import           Test.Tasty                          (TestTree, testGroup)
import           Test.Tasty.Hspec                    (describe, it, shouldBe,
                                                      testSpec)
import           Utils.Base                          (Except (..))


successTestLexer = describe "Success Lexer tests:" $ do
  it (show SLT.lexerTest1) $ testTokenize SLT.lexerTest1 `shouldBe` SLT.expectedLexerTest1
  it (show SLT.lexerTest2) $ testTokenize SLT.lexerTest2 `shouldBe` SLT.expectedLexerTest2
  it (show SLT.lexerTest3) $ testTokenize SLT.lexerTest3 `shouldBe` SLT.expectedLexerTest3
  it (show SLT.lexerTest5) $ testTokenize SLT.lexerTest5 `shouldBe` SLT.expectedLexerTest5
  it (show SLT.lexerTest6) $ testTokenize SLT.lexerTest6 `shouldBe` SLT.expectedLexerTest6
  it (show SLT.lexerTest7) $ testTokenize SLT.lexerTest7 `shouldBe` SLT.expectedLexerTest7
  it (show SLT.lexerTest8) $ testTokenize SLT.lexerTest8 `shouldBe` SLT.expectedLexerTest8

successTestParser = describe "Success Parser tests:" $ do
  it (show SPT.parserTest1) $ evaluate SPT.parserTest1 `shouldBe` SPT.expectedParserTest1
  it (show SPT.parserTest2) $ evaluate SPT.parserTest2 `shouldBe` SPT.expectedParserTest2
  it (show SPT.parserTest3) $ evaluate SPT.parserTest3 `shouldBe` SPT.expectedParserTest3

spec_Success :: IO TestTree
spec_Success = testSpec "Kotlin Array tests" $ do
  successTestLexer
  successTestParser


testTokenize :: String -> Except e [Token]
testTokenize x = Success $ tokenize x
