                                                                                 
module GeneratedKotlinArray.Lexer
  ( Token (..)
  , tokenize
  ) where

                                                                              
                                                                                
import           Control.Applicative     (many, (<|>))                          
import           Utils.Base              (Except (..), ParseError (..), Parser, 
                                          run)                                  
import           Utils.ParserCombinators (pcEof, pcRegExp, pcSkipSpaces)        
                                                                                
tokenParse1 = (const VAR ) <$> pcRegExp "^var"

tokenParse2 = (const ARRAY ) <$> pcRegExp "^Array"

tokenParse3 = (const OPEN_CHEVRONS ) <$> pcRegExp "^<"

tokenParse4 = (const CLOSE_CHEVRONS ) <$> pcRegExp "^>"

tokenParse5 = (const SEMICOLON ) <$> pcRegExp "^;"

tokenParse6 = (const COLON ) <$> pcRegExp "^:"

tokenParse7 = (const COMMA ) <$> pcRegExp "^,"

tokenParse8 = (WORD ) <$> pcRegExp "^[a-zA-Z0-9]+"
                                                                              
                                                                                
commonLexer = tokenParse1 <|> tokenParse2 <|> tokenParse3 <|> tokenParse4 <|> tokenParse5 <|> tokenParse6 <|> tokenParse7 <|> tokenParse8                                                                
                                                                                
lexer  = many $ pcSkipSpaces commonLexer                                            
                                                                                
data Token
  = VAR
  | WORD String
  | ARRAY
  | OPEN_CHEVRONS
  | CLOSE_CHEVRONS
  | COLON
  | SEMICOLON
  | COMMA
  | TokenEndInput
  | Epsilon

tokenize :: String -> [Token]
tokenize str = case run (lexer <* pcEof) str of
  Success r            -> r
  Error (ErrorAtPos e) -> error $ show e





instance Eq Token where
  VAR == VAR                       = True
  (WORD x) == (WORD y)             = x == y
  ARRAY == ARRAY                   = True
  OPEN_CHEVRONS == OPEN_CHEVRONS   = True
  CLOSE_CHEVRONS == CLOSE_CHEVRONS = True
  SEMICOLON == SEMICOLON           = True
  COLON == COLON                   = True
  COMMA == COMMA                   = True
  _ == _                           = False

instance Show Token where
  show VAR            = "VAR"
  show (WORD a)       = "WORD " ++ "\\\"" ++ a ++ "\\\""
  show ARRAY          = "ARRAY"
  show OPEN_CHEVRONS  = "OPEN_CHEVRONS"
  show CLOSE_CHEVRONS = "CLOSE_CHEVRONS"
  show SEMICOLON      = "SEMICOLON"
  show COLON          = "COLON"
  show COMMA          = "COMMA"
  show Epsilon        = "Epsilon"                                                                            
                                                                              