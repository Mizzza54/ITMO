{-# LANGUAGE BlockArguments #-}
module GeneratedKotlinArray.Parser (evaluate, Tree (..)) where

import GeneratedKotlinArray.Lexer (Token(..), tokenize)


import           Control.Applicative       ((<|>))
import           Utils.Base                (Annotated (..), Except (..),
                                            ExceptState (..), ParseError (..),
                                            Parser, modifyExceptState, run)
import           Utils.ParserCombinators   (pcEof, pcSatisfyPredicate)

data Data  = Attributes
  { _res :: Tree 
  }


token1 a0 = do
  a1 <- pcSatisfyPredicate func
  return ()
  where func (TokenEndInput) = True
        func _ = False

token2 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a1 )
  where func (VAR ) = True
        func _ = False

token3 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a1 )
  where func (ARRAY ) = True
        func _ = False

token4 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a1 )
  where func (OPEN_CHEVRONS ) = True
        func _ = False

token5 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a1 )
  where func (CLOSE_CHEVRONS ) = True
        func _ = False

token6 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a1 )
  where func (SEMICOLON ) = True
        func _ = False

token7 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a1 )
  where func (COLON ) = True
        func _ = False

token8 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a1 )
  where func (WORD _ ) = True
        func _ = False


parserD a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    VAR  -> do 
      a1<- token2 (a0)
      a2<- token8 (a0)
      a3<- token7 (a0)
      a4<- token3 (a0)
      a5<- token4 (a0)
      a6<- parserT (a0)
      a7<- token5 (a0)
      a8<- token6 (a0)
      return (Attributes $ Node "D" [Leaf a1, Leaf a2, Leaf a3, Leaf a4, Leaf a5, a6, Leaf a7, Leaf a8] )


parserT a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    WORD _  -> do 
      a1<- token8 (a0)
      return (Node "T" [Leaf a1] )

    ARRAY  -> do 
      a1<- token3 (a0)
      a2<- token4 (a0)
      a3<- parserT (a0)
      a4<- token5 (a0)
      return (Node "T" [Leaf a1, Leaf a2, a3, Leaf a4] )




parser  x = run (_res <$> (parserD x) <* waitTokenEndInput)

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

token2word :: Token -> String
token2word (WORD x) = x

evaluateTokens = parser VAR

evaluate :: String -> Except ParseError Tree
evaluate = evaluateTokens . tokenize





data Tree = Node String [Tree] | Leaf Token deriving (Show, Eq)

isWord :: Token -> Bool
isWord t = case t of
  WORD _ -> True
  _ -> False

