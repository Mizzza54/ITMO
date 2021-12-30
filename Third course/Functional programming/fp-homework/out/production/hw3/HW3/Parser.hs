module HW3.Parser where

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = undefined

token :: MonadParsec e s m
  => (Token s -> Maybe a) -- ^ Matching function for the token to parse
  -> Set (ErrorItem (Token s)) -- ^ Expected items (in case of an error)
  -> m a