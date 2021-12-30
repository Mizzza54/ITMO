module GeneratedCalculator.Lexer
  ( Token (..)
  , tokenize
  ) where

                                                                              
                                                                                
import           Control.Applicative     (many, (<|>))                          
import           Utils.Base              (Except (..), ParseError (..), Parser, 
                                          run)                                  
import           Utils.ParserCombinators (pcEof, pcRegExp, pcSkipSpaces)        
                                                                                
tokenParse1 = (TokenNum . read ) <$> pcRegExp "^[0-9]+"

tokenParse2 = (const TokenSum ) <$> pcRegExp "^\\+"

tokenParse3 = (const TokenSub ) <$> pcRegExp "^-"

tokenParse4 = (const TokenMul ) <$> pcRegExp "^\\*"

tokenParse5 = (const TokenDiv ) <$> pcRegExp "^/"

tokenParse6 = (const TokenOB ) <$> pcRegExp "^\\("

tokenParse7 = (const TokenCB ) <$> pcRegExp "^\\)"

tokenParse8 = (const TokenSP ) <$> pcRegExp "^[\t\r\n]+"
                                                                              
                                                                                
commonLexer = tokenParse1 <|> tokenParse2 <|> tokenParse3 <|> tokenParse4 <|> tokenParse5 <|> tokenParse6 <|> tokenParse7 <|> tokenParse8                                                                
                                                                                
lexer  = many $ pcSkipSpaces commonLexer                                            
                                                                                
data Token
 = TokenSum
 | TokenSub
 | TokenMul
 | TokenDiv
 | TokenOB
 | TokenCB
 | TokenSP
 | TokenNum Double
 deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize str = case run (lexer <* pcEof) str of
  Success r            -> r
  Error (ErrorAtPos e) -> error $ show e
                                                                              
