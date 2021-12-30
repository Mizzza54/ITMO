module Unit where

import           Data.Char            (isSpace)
import           Parser               (parse)
import qualified SuccessSimpleTests   as SST
import qualified SuccessSubIndexTests as SSubIT
import qualified SuccessSupIndexTests as SSupIT
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
    it SSubIT.test1 $ removeEscapeChars (show $ parse SSubIT.test1) `shouldBe` SSubIT.expected1
    it SSubIT.test2 $ removeEscapeChars (show $ parse SSubIT.test2) `shouldBe` SSubIT.expected2
    it SSubIT.test3 $ removeEscapeChars (show $ parse SSubIT.test3) `shouldBe` SSubIT.expected3
    it SSubIT.test4 $ removeEscapeChars (show $ parse SSubIT.test4) `shouldBe` SSubIT.expected4
    it SSubIT.test5 $ removeEscapeChars (show $ parse SSubIT.test5) `shouldBe` SSubIT.expected5
    it SSubIT.test6 $ removeEscapeChars (show $ parse SSubIT.test6) `shouldBe` SSubIT.expected6    
    it SSubIT.test7 $ removeEscapeChars (show $ parse SSubIT.test7) `shouldBe` SSubIT.expected7
    it SSubIT.test8 $ removeEscapeChars (show $ parse SSubIT.test8) `shouldBe` SSubIT.expected8  
    it SSubIT.test9 $ removeEscapeChars (show $ parse SSubIT.test9) `shouldBe` SSubIT.expected9 
    it SSubIT.test10 $ removeEscapeChars (show $ parse SSubIT.test10) `shouldBe` SSubIT.expected10
  describe "Sup index:" $ do
    it SSupIT.test1 $ removeEscapeChars (show $ parse SSupIT.test1) `shouldBe` SSupIT.expected1
    it SSupIT.test2 $ removeEscapeChars (show $ parse SSupIT.test2) `shouldBe` SSupIT.expected2
    it SSupIT.test3 $ removeEscapeChars (show $ parse SSupIT.test3) `shouldBe` SSupIT.expected3
    it SSupIT.test4 $ removeEscapeChars (show $ parse SSupIT.test4) `shouldBe` SSupIT.expected4
    it SSupIT.test5 $ removeEscapeChars (show $ parse SSupIT.test5) `shouldBe` SSupIT.expected5
    it SSupIT.test6 $ removeEscapeChars (show $ parse SSupIT.test6) `shouldBe` SSupIT.expected6    
    it SSupIT.test7 $ removeEscapeChars (show $ parse SSupIT.test7) `shouldBe` SSupIT.expected7
    it SSupIT.test8 $ removeEscapeChars (show $ parse SSupIT.test8) `shouldBe` SSupIT.expected8  
    it SSupIT.test9 $ removeEscapeChars (show $ parse SSupIT.test9) `shouldBe` SSupIT.expected9 
    it SSupIT.test10 $ removeEscapeChars (show $ parse SSupIT.test10) `shouldBe` SSupIT.expected10
    
        
removeEscapeChars :: String -> String
removeEscapeChars = filter $ not . isSpace

hspecTestTree :: IO TestTree
hspecTestTree = do
  success <- spec_Success
  return $ testGroup "All tests:" [success]
