module GeneratedTex2Html.Lexer
  ( Token (..)
  , tokenize
  ) where

                                                                              
                                                                                
import           Control.Applicative     (many, (<|>))                          
import           Utils.Base              (Except (..), ParseError (..), Parser, 
                                          run)                                  
import           Utils.ParserCombinators (pcEof, pcRegExp, pcSkipSpaces)        
                                                                                
tokenParse1 = (const TokenOpenCurlyBrace ) <$> pcRegExp "^{"

tokenParse2 = (const TokenCloseCurlyBrace ) <$> pcRegExp "^}"

tokenParse3 = (const TokenDollar ) <$> pcRegExp "^\\$"

tokenParse4 = (const TokenCircumflexus ) <$> pcRegExp "^\\^"

tokenParse5 = (const TokenUnderline ) <$> pcRegExp "^_"

tokenParse6 = (TokenWord ) <$> pcRegExp "^[a-zA-Z0-9=+*-/]"
                                                                              
                                                                                
commonLexer = tokenParse1 <|> tokenParse2 <|> tokenParse3 <|> tokenParse4 <|> tokenParse5 <|> tokenParse6                                                                
                                                                                
lexer  = many $ pcSkipSpaces commonLexer                                            
                                                                                
data Token
  = TokenBackslash
  | TokenOpenCurlyBrace
  | TokenCloseCurlyBrace
  | TokenDollar
  | TokenCircumflexus
  | TokenUnderline
  | TokenWord String
  | TokenEOF
  | TokenWhiteSpace
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize str = case run (lexer <* pcEof) str of
  Success r            -> r
  Error (ErrorAtPos e) -> error $ show e
                                                                              
