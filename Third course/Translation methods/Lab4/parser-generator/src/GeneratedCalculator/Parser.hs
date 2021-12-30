{-# LANGUAGE BlockArguments #-}
module GeneratedCalculator.Parser (evaluate) where

import GeneratedCalculator.Lexer (Token(..), tokenize)


import           Control.Applicative       ((<|>))
import           Utils.Base                (Annotated (..), Except (..),
                                            ExceptState (..), ParseError (..),
                                            Parser, modifyExceptState, run)
import           Utils.ParserCombinators   (pcEof, pcSatisfyPredicate)

data Data  = Attributes
  { _res :: Double 
  , _acc :: Double 
  }


token1 a0 = do
  a1 <- pcSatisfyPredicate func
  return ()
  where func (TokenEndInput) = True
        func _ = False

token2 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a0 )
  where func (TokenSum ) = True
        func _ = False

token3 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a0 )
  where func (TokenSub ) = True
        func _ = False

token4 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a0 )
  where func (TokenMul ) = True
        func _ = False

token5 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a0 )
  where func (TokenDiv ) = True
        func _ = False

token6 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a0 )
  where func (TokenOB ) = True
        func _ = False

token7 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a0 )
  where func (TokenCB ) = True
        func _ = False

token8 a0 = do
  a1 <- pcSatisfyPredicate func
  return (Attributes { _res = (token2num a1) } )
  where func (TokenNum _) = True
        func _ = False


parserE a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenOB  -> do 
      a1<- parserT (a0)
      a2<- parserET (Attributes { _acc = _res a1 } )
      return (Attributes { _res = _res a2 })

    TokenNum _ -> do 
      a1<- parserT (a0)
      a2<- parserET (Attributes { _acc = _res a1 } )
      return (Attributes { _res = _res a2 })


parserET a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenSum  -> do 
      a1<- token2 (a0)
      a2<- parserT (a0)
      a3<- parserET (Attributes { _acc = _acc a0 + _res a2 } )
      return (Attributes { _res = _res a3 })

    TokenSub  -> do 
      a1<- token3 (a0)
      a2<- parserT (a0)
      a3<- parserET (Attributes { _acc = _acc a0 - _res a2 } )
      return (Attributes { _res = _res a3 })

    TokenCB  -> do 
      
      return (Attributes { _res = _acc a0 } )

    TokenEndInput -> do 
      
      return (Attributes { _res = _acc a0 } )


parserT a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenOB  -> do 
      a1<- parserF (a0)
      a2<- parserTP (Attributes { _acc = _res a1 } )
      return (Attributes { _res = _res a2 })

    TokenNum _ -> do 
      a1<- parserF (a0)
      a2<- parserTP (Attributes { _acc = _res a1 } )
      return (Attributes { _res = _res a2 })


parserTP a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenMul  -> do 
      a1<- token4 (a0)
      a2<- parserF (a0)
      a3<- parserTP (Attributes { _acc = _acc a0 * _res a2 } )
      return (Attributes { _res = _res a3 })

    TokenDiv  -> do 
      a1<- token5 (a0)
      a2<- parserF (a0)
      a3<- parserTP (Attributes { _acc = _acc a0 / _res a2 } )
      return (Attributes { _res = _res a3 })

    TokenCB  -> do 
      
      return (Attributes { _res = _acc a0 } )

    TokenSum  -> do 
      
      return (Attributes { _res = _acc a0 } )

    TokenSub  -> do 
      
      return (Attributes { _res = _acc a0 } )

    TokenEndInput -> do 
      
      return (Attributes { _res = _acc a0 } )


parserF a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenNum _ -> do 
      a1<- token8 (a0)
      return (Attributes { _res = _res a1 } )

    TokenOB  -> do 
      a1<- token6 (a0)
      a2<- parserE (a0)
      a3<- token7 (a0)
      return (Attributes { _res = _res a2 } )




parser  x = run (_res <$> (parserE x) <* waitTokenEndInput)

nextToken :: Parser [Token] Token
nextToken = ES \(pos, s) -> case s of
  []     -> Success (TokenEndInput :# (pos, []))
  (c:cs) -> Success (c :# (pos + 1, cs))

rollback Epsilon = id
rollback foo = \x -> (fst x - 1, foo : snd x)

waitTokenEndInput :: Parser [Token] ()
waitTokenEndInput = ES \(pos, s) ->
  case s of
    [TokenEndInput] -> Success (() :# (pos, []))
    []              -> Success (() :# (pos, []))
    _               -> Error (ErrorAtPos pos)

token2num :: Token -> Double
token2num (TokenNum x) = x

evaluateTokens :: [Token] -> Except ParseError Double
evaluateTokens = parser (Attributes 0 0)

evaluate :: String -> Except ParseError Double
evaluate = evaluateTokens . tokenize


