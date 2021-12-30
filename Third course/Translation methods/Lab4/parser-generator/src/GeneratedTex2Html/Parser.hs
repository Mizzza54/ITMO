{-# LANGUAGE BlockArguments #-}
module GeneratedTex2Html.Parser (evaluate) where

import GeneratedTex2Html.Lexer (Token(..), tokenize)


import           Control.Applicative       ((<|>))
import           Utils.Base                (Annotated (..), Except (..),
                                            ExceptState (..), ParseError (..),
                                            Parser, modifyExceptState, run)
import           Utils.ParserCombinators   (pcEof, pcSatisfyPredicate)

data AttributeData  = Attributes
  { _res :: Expressions 
  }


token1 a0 = do
  a1 <- pcSatisfyPredicate func
  return ()
  where func (TokenEndInput) = True
        func _ = False

token2 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a0 )
  where func (TokenOpenCurlyBrace ) = True
        func _ = False

token3 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a0 )
  where func (TokenCloseCurlyBrace ) = True
        func _ = False

token4 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a0 )
  where func (TokenDollar ) = True
        func _ = False

token5 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a0 )
  where func (TokenCircumflexus ) = True
        func _ = False

token6 a0 = do
  a1 <- pcSatisfyPredicate func
  return (a0 )
  where func (TokenUnderline ) = True
        func _ = False

token7 a0 = do
  a1 <- pcSatisfyPredicate func
  return (token2word a1 )
  where func (TokenWord _ ) = True
        func _ = False


parserStart a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenDollar  -> do 
      a1<- parserExpressions (a0)
      return (Attributes $ Expressions a1 )

    Epsilon -> do 
      a1<- parserExpressions (a0)
      return (Attributes $ Expressions a1 )


parserExpressions a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenDollar  -> do 
      a1<- parserExpression (a0)
      a2<- parserExpressions (a0)
      return ((a1 : a2) )

    TokenEndInput -> do 
      
      return ([] )


parserExpression a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenDollar  -> do 
      a1<- token4 (a0)
      a2<- parserFormulaList (a0)
      a3<- token4 (a0)
      return (ExpressionFormula a2 )


parserFormulaList a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenWord _  -> do 
      a1<- parserFormula (a0)
      a2<- parserFormulaList (a0)
      return (a1 : a2 )

    TokenDollar  -> do 
      
      return ([] )

    TokenCloseCurlyBrace  -> do 
      
      return ([] )


parserFormula a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenWord _  -> do 
      a1<- parserData (a0)
      a2<- parserSubAndSup (a0)
      return (Formula a1 (fst a2) (snd a2) )


parserData a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenWord _  -> do 
      a1<- parserSimpleWord (a0)
      return (SimpleWord a1 )


parserSimpleWord a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenWord _  -> do 
      a1<- token7 (a0)
      return (a1 )


parserSubAndSup a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenUnderline  -> do 
      a1<- parserSub (a0)
      a2<- parserNextSub (a0)
      return ((a1, a2) )

    TokenCircumflexus  -> do 
      a1<- parserSup (a0)
      a2<- parserNextSup (a0)
      return ((a2, a1) )

    TokenDollar  -> do 
      
      return ((Nothing, Nothing) )

    TokenCloseCurlyBrace  -> do 
      
      return ((Nothing, Nothing) )

    TokenWord _  -> do 
      
      return ((Nothing, Nothing) )


parserNextSub a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenCircumflexus  -> do 
      a1<- parserSup (a0)
      return (a1 )

    TokenDollar  -> do 
      
      return (Nothing )

    TokenCloseCurlyBrace  -> do 
      
      return (Nothing )

    TokenWord _  -> do 
      
      return (Nothing )


parserNextSup a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenUnderline  -> do 
      a1<- parserSub (a0)
      return (a1 )

    TokenDollar  -> do 
      
      return (Nothing )

    TokenCloseCurlyBrace  -> do 
      
      return (Nothing )

    TokenWord _  -> do 
      
      return (Nothing )


parserSub a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenUnderline  -> do 
      a1<- token6 (a0)
      a2<- parserContent (a0)
      return (Just (Formulas a2) )


parserSup a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenCircumflexus  -> do 
      a1<- token5 (a0)
      a2<- parserContent (a0)
      return (Just (Formulas a2) )


parserContent a0 = do
  foo <- nextToken
  modifyExceptState (rollback foo)
  case foo of
    TokenOpenCurlyBrace  -> do 
      a1<- token2 (a0)
      a2<- parserFormulaList (a0)
      a3<- token3 (a0)
      return (a2 )

    TokenWord _  -> do 
      a1<- parserData (a0)
      return ([(Formula a1 Nothing Nothing)] )




parser  x = run (_res <$> (parserStart x) <* waitTokenEndInput)

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
token2word (TokenWord x) = x

evaluateTokens :: [Token] -> Except ParseError Expressions
evaluateTokens = parser (Attributes $ Expressions [])

evaluate :: String -> Expressions
evaluate str = case evaluateTokens . tokenize $ str of
  Success r -> r
  Error e -> error $ show e





newtype Expressions = Expressions [Expression]

newtype Expression = ExpressionFormula [Formula]

data Formula = Formula Data (Maybe Formulas) (Maybe Formulas)

newtype Formulas = Formulas [Formula]

data Data
  = SimpleChar String
  | SimpleWord String
  | Complex String [Expression]

instance Show Expressions where
  show (Expressions array) = unlines $ map show array

instance Show Expression where
  show (ExpressionFormula array) = "<math>\n" ++ showFormulas array ++ "</math>" where
    showFormulas = foldl join ""
    join b a = b ++ show a ++ "\n"

instance Show Formula where
  show (Formula body Nothing Nothing)
    =  "<mo>"
    ++ show body
    ++ "</mo>"
  show (Formula body Nothing (Just sup))
    =  "<msup>"
    ++ "<mo>"
    ++ show body
    ++ "</mo>"
    ++ "<mn>"
    ++ show sup
    ++ "</mn>"
    ++ "</msup>"
  show (Formula body (Just sub) Nothing)
    =  "<msub>"
    ++ "<mo>"
    ++ show body
    ++ "</mo>"
    ++ "<mn>"
    ++ show sub
    ++ "</mn>"
    ++ "</msub>"
  show (Formula body (Just sub) (Just sup))
    =  "<msubsup>"
    ++ "<mo>"
    ++ show body
    ++ "</mo>"
    ++ "<mn>"
    ++ show sub
    ++ "</mn>"
    ++ "<mi>"
    ++ show sup
    ++ "</mi>"
    ++ "</msubsup>"

instance Show Formulas where
  show (Formulas array) = unlines $ map show array

instance Show Data where
  show (SimpleChar ch)     = ch
  show (SimpleWord word)   = word
  show (Complex str exprs) = "Complex ????"


