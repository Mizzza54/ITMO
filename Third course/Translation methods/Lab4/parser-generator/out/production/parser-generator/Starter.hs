module Starter where

import           LexerGenerator.Core
import           ParserGenerator.Core
import           System.IO            (IOMode (..), hClose, hGetContents,
                                       openFile)
import           Utils.Base

--runFile2 fromFile toFile pars =
--  do
--    handle <- openFile fromFile ReadMode
--    everything <- hGetContents handle
--    putStrLn  (show (runParser pars everything))
--    writeFile toFile (show . fst . fromJust $ runParser pars everything)
--    hClose handle

runFile fromFile toFile = do
  handle <- openFile fromFile ReadMode
  everything <- hGetContents handle
  let result = run runParseLexicalFile everything
  case result of
    Success r -> writeFile toFile (show r)
    Error (ErrorAtPos e)   -> print e
  hClose handle
  
runFile2 fromFile toFile = do
  handle <- openFile fromFile ReadMode
  everything <- hGetContents handle
  let result = run runParseGrammarFile everything
  case result of
    Success r -> writeFile toFile (show r)
    Error (ErrorAtPos e)   -> print e
  hClose handle