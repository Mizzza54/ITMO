import           Test.Tasty (defaultMain)
import           Unit

main :: IO ()
main = do
  test <- hspecTestTree
  defaultMain test
