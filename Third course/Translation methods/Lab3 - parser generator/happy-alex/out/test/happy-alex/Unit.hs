module Unit where

import           Data.Char            (isSpace)
import           Parser               (parse)
import qualified SuccessSimpleTests   as SST
import qualified SuccessSubIndexTests as SSIT
import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.Hspec     (Spec, describe, it, shouldBe,
                                       shouldSatisfy, testSpec)

spec_Success :: IO TestTree
spec_Success = testSpec "Success tests:" $ do
  describe "Simple formulas:" $ do
    it SST.test1 $ removeEscapeChars (show $ parse SST.test1) `shouldBe` SST.expected1
    it SST.test2 $ removeEscapeChars (show $ parse SST.test2) `shouldBe` SST.expected2
    it SST.test3 $ removeEscapeChars (show $ parse SST.test3) `shouldBe` SST.expected3
    it SST.test4 $ removeEscapeChars (show $ parse SST.test4) `shouldBe` SST.expected4
    it SST.test5 $ removeEscapeChars (show $ parse SST.test5) `shouldBe` SST.expected5
  describe "Sub index:" $ do
    it SSIT.test1 $ removeEscapeChars (show $ parse SSIT.test1) `shouldBe` SSIT.expected1
    it SSIT.test2 $ removeEscapeChars (show $ parse SSIT.test2) `shouldBe` SSIT.expected2
    it SSIT.test3 $ removeEscapeChars (show $ parse SSIT.test3) `shouldBe` SSIT.expected3
    it SSIT.test4 $ removeEscapeChars (show $ parse SSIT.test4) `shouldBe` SSIT.expected4
    it SSIT.test5 $ removeEscapeChars (show $ parse SSIT.test5) `shouldBe` SSIT.expected5

removeEscapeChars :: String -> String
removeEscapeChars = filter $ not . isSpace

hspecTestTree :: IO TestTree
hspecTestTree = do
  success <- spec_Success
  return $ testGroup "All tests:" [success]
