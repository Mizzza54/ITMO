module GeneratedKotlinArray.Lexer
  ( Token (..)
  , tokenize
  ) where

                                                                              
                                                                                
import           Control.Applicative     (many, (<|>))                          
import           Utils.Base              (Except (..), ParseError (..), Parser, 
                                          run)                                  
import           Utils.ParserCombinators (pcEof, pcRegExp, pcSkipSpaces)        
                                                                                
tokenParse1 = (const TokenVar ) <$> pcRegExp "^var"

tokenParse2 = (const TokenArray ) <$> pcRegExp "^Array"

tokenParse3 = (const TokenOC ) <$> pcRegExp "^<"

tokenParse4 = (const TokenCC ) <$> pcRegExp "^>"

tokenParse5 = (const TokenSemicolon ) <$> pcRegExp "^;"

tokenParse6 = (const TokenColon ) <$> pcRegExp "^:"

tokenParse7 = (const TokenComma ) <$> pcRegExp "^,"

tokenParse8 = (TokenWord ) <$> pcRegExp "^[a-zA-Z0-9]+"
                                                                              
                                                                                
commonLexer = tokenParse1 <|> tokenParse2 <|> tokenParse3 <|> tokenParse4 <|> tokenParse5 <|> tokenParse6 <|> tokenParse7 <|> tokenParse8                                                                
                                                                                
lexer  = many $ pcSkipSpaces commonLexer                                            
                                                                                
data Token
  = TokenVar
  | TokenWord String
  | TokenArray
  | TokenOC
  | TokenCC
  | TokenSemicolon
  | TokenColon
  | TokenComma
 deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize str = case run (lexer <* pcEof) str of
  Success r            -> r
  Error (ErrorAtPos e) -> error $ show e
                                                                              
