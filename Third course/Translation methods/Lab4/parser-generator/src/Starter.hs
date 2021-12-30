module Starter
 ( generateCalculator
 , generateKotlinArray
 , generateTex2Html
 ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           LexerGenerator.Core
import           ParserGenerator.Core
import           System.IO            (IOMode (..), hClose, hGetContents,
                                       openFile)
import           Utils.Base

----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

runGenerate :: Show a => FilePath -> FilePath -> Parser String a -> IO ()
runGenerate fromFile toFile runner = do
  handle <- openFile fromFile ReadMode
  content <- hGetContents handle
  let result = run runner content
  case result of
    Success r            -> writeFile toFile (show r)
    Error (ErrorAtPos e) -> print e
  hClose handle

----------------------------------------------------------------------
-- Run generate
----------------------------------------------------------------------

generateCalculator :: IO ()
generateCalculator = do
  runGenerate "../resources/Calculator/Lexer.mya" "./GeneratedCalculator/Lexer.hs" runParseLexicalFile
  runGenerate "../resources/Calculator/Parser.myy" "./GeneratedCalculator/Parser.hs" runParseGrammarFile

generateKotlinArray :: IO ()
generateKotlinArray = do
  runGenerate "../resources/KotlinArray/Lexer.mya" "./GeneratedKotlinArray/Lexer.hs" runParseLexicalFile
  runGenerate "../resources/KotlinArray/Parser.myy" "./GeneratedKotlinArray/Parser.hs" runParseGrammarFile

generateTex2Html :: IO ()
generateTex2Html = do
  runGenerate "../resources/Tex2Html/Lexer.mya" "./GeneratedTex2Html/Lexer.hs" runParseLexicalFile
  runGenerate "../resources/Tex2Html/Parser.myy" "./GeneratedTex2Html/Parser.hs" runParseGrammarFile
