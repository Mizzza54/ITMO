{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------    
  
import Control.Monad.Cont (lift, liftIO)

import qualified Data.Set as Set

import System.Console.Haskeline

import HW3.Action (HIO (..), HiPermission (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue, prettyError)
import Prettyprinter.Render.Terminal (putDoc)
import Prettyprinter (pretty)
import Text.Megaparsec (errorBundlePretty)

----------------------------------------------------------------------
-- Main
----------------------------------------------------------------------  

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just "" -> loop
        Just input -> do
          case parse input of
            Left parserError  -> lift . putDoc . pretty . errorBundlePretty $ parserError
            Right parserValue -> do
              res <- liftIO $ runHIO (eval parserValue) (Set.fromList [AllowRead, AllowWrite, AllowTime]) 
              lift . putDoc $ case res of
                Left e'  -> prettyError e' <> "\n"
                Right e' -> prettyValue e' <> "\n"
          loop