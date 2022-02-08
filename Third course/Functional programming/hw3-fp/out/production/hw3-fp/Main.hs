module Main where

import           System.Console.Haskeline
import HW3.Parser (parse)
import HW3.Evaluator (eval)
import HW3.Pretty (prettyValue)
import HW3.Base
import Data.Ratio ((%))
import HW3.Action (HIO (..), HiPermission (..))
import Control.Monad.Cont (liftIO)
import qualified Data.Set as Set


--{-# LANGUAGE BlockArguments #-}
--
--module Main where
--
--import HW3.Parser
--import HW3.Evaluator
--import HW3.Pretty
--import HW3.Action
--import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine, getExternalPrint, outputStrLn)
--import Control.Monad.Cont (lift, liftIO)
--import Prettyprinter.Render.Terminal (putDoc)
--import Prettyprinter
--import Text.Megaparsec.Error (errorBundlePretty)
--import Data.Set (fromList)
--
--main :: IO ()
--main = runInputT defaultSettings loop
--         where
--           loop :: InputT IO ()
--           loop = do
--             minput <- getInputLine "hi> "
--             case minput of
--               Nothing -> return ()
--               Just input -> do
--                 case parse input of
--                   Left e  -> outputStrLn $ show e
--                   Right e -> do
--                     res <- liftIO $ runHIO (eval e) (fromList [AllowRead, AllowWrite])
--                     case res of
--                       Left e'  -> outputStrLn $ show e'
--                       Right e' -> outputStrLn $ show e'
--                 loop

main :: IO ()
main = runInputT defaultSettings $ loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return()
        Just ":q" -> return ()
        Just "" -> loop
        Just input -> do
          let parsed = parse input
          case parsed of
            Left parseError -> outputStrLn $ show parseError
            Right parseValue -> do
              v <- liftIO $ runHIO (eval parseValue) (Set.fromList [AllowRead, AllowWrite, AllowTime])
              case v of
                Left evalError  -> outputStrLn $  show evalError
                Right evalValue -> outputStrLn $  show (prettyValue evalValue)
          loop




--main = runInputT defaultSettings loop
--   where
--       loop :: InputT IO ()
--       loop = do
--           minput <- getInputLine "hi> "
--           case minput of
--               Nothing -> return ()
--               Just "quit" -> return ()
--               Just "" -> loop
--               Just input -> do
--                 outputStrLn $ case parse input of
--                   Left parseErrorBundle -> show parseErrorBundle
--                   Right hiExpr -> do
--                     case eval hiExpr of
--                       Left hiError -> show hiError
--                       Right hiValue -> show (prettyValue (HiValueNumber (10 % 20)))
--                 loop



--main :: IO ()
--main = runInputT defaultSettings loop
--   where
--       loop :: InputT IO ()
--       loop = do
--           minput <- getInputLine "hi> "
--           case minput of
--               Nothing -> return ()
--               Just "quit" -> return ()  -- outputStrLn $ show $ prettyValue $ eval $ parse input
--               Just input -> do
--                 case parse input of
--                   Left parseErrorBundle -> do outputStrLn (show parseErrorBundle)
--                   Right hiExpr -> do
--                     case eval hiExpr of
--                       Left hiError -> do outputStrLn (show hiError)
--                       Right hiValue -> do outputStrLn $ show (prettyValue (HiValueNumber (10 % 20)))
--                                        loop
