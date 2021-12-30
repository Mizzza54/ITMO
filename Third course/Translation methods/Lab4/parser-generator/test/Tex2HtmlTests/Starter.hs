module Tex2HtmlTests.Starter where

import           Data.Char                          (isSpace)
import           GeneratedTex2Html.Parser           (evaluate)
import           Test.Tasty                         (TestTree, testGroup)
import           Test.Tasty.Hspec                   (Spec, describe, it,
                                                     shouldBe, shouldSatisfy,
                                                     testSpec)
import qualified Tex2HtmlTests.SuccessSimpleTests   as SST
import qualified Tex2HtmlTests.SuccessSubIndexTests as SSubIT

spec_Success :: IO TestTree
spec_Success = testSpec "Tex2Html tests" $ do
  describe "Simple formulas:" $ do
    it SST.test1 $ removeEscapeChars (show $ evaluate SST.test1) `shouldBe` SST.expected1
    it SST.test2 $ removeEscapeChars (show $ evaluate SST.test2) `shouldBe` SST.expected2
    it SST.test3 $ removeEscapeChars (show $ evaluate SST.test3) `shouldBe` SST.expected3
    it SST.test4 $ removeEscapeChars (show $ evaluate SST.test4) `shouldBe` SST.expected4
    it SST.test5 $ removeEscapeChars (show $ evaluate SST.test5) `shouldBe` SST.expected5
  describe "Sub index:" $ do
    it SSubIT.test1 $ removeEscapeChars (show $ evaluate SSubIT.test1) `shouldBe` SSubIT.expected1
    it SSubIT.test2 $ removeEscapeChars (show $ evaluate SSubIT.test2) `shouldBe` SSubIT.expected2
    it SSubIT.test3 $ removeEscapeChars (show $ evaluate SSubIT.test3) `shouldBe` SSubIT.expected3
    it SSubIT.test4 $ removeEscapeChars (show $ evaluate SSubIT.test4) `shouldBe` SSubIT.expected4
    it SSubIT.test5 $ removeEscapeChars (show $ evaluate SSubIT.test5) `shouldBe` SSubIT.expected5
    it SSubIT.test6 $ removeEscapeChars (show $ evaluate SSubIT.test6) `shouldBe` SSubIT.expected6

removeEscapeChars :: String -> String
removeEscapeChars = filter $ not . isSpace
