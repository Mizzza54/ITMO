import           KotlinArrayTests.Starter (spec_Success)
import           Test.Tasty               (TestTree, defaultMain, testGroup)
import qualified Tex2HtmlTests.Starter    (spec_Success)

main :: IO ()
main = do
  test <- hspecTestTree
  defaultMain test

hspecTestTree :: IO TestTree
hspecTestTree = do
  success <- Tex2HtmlTests.Starter.spec_Success
  success <- KotlinArrayTests.Starter.spec_Success
  return $ testGroup "All tests:" [success]
