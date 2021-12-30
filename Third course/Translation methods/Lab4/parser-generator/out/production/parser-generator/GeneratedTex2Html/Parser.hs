                                                                
module GeneratedTex2Html.Parser (evaluate) where

import GeneratedTex2Html.Lexer (Token(..), tokenize)
                                                              
                                                                
import           Control.Applicative     ((<|>))                
import           Utils.Base              (Except (..), ParseError (..), Parser,   
                                          run)                                    
import           Utils.ParserCombinators (pcSatisfyPredicate, pcEof)   
                                                                
data AttributeData  = Attributes      
  { _res :: Expressions                     
  }                       
                          
                                                              
                                                                
token1 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (a0 )                      
    where func (TokenOpenCurlyBrace ) = True          
          func _ = False
token2 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (a0 )                      
    where func (TokenCloseCurlyBrace ) = True          
          func _ = False
token3 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (a0 )                      
    where func (TokenDollar ) = True          
          func _ = False
token4 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (a0 )                      
    where func (TokenCircumflexus ) = True          
          func _ = False
token5 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (a0 )                      
    where func (TokenUnderline ) = True          
          func _ = False
token6 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (token2word a1 )                      
    where func (TokenWord _ ) = True          
          func _ = False                                                              
                                                                
-- | Generated parsers                                          
                                                                
                          
parserStart a0 =                     
  do 
    a1<- parserExpressions (a0)                    
    return (Attributes $ Expressions a1 )                          

                          
parserExpressions a0 =                     
  do 
    a1<- parserExpression (a0)
    a2<- parserExpressions (a0)                    
    return ((a1 : a2) )
  <|>
  do                     
    return ([] )                          

                          
parserExpression a0 =                     
  do 
    a1<- token3 (a0)
    a2<- parserFormulaList (a0)
    a3<- token3 (a0)                    
    return (ExpressionFormula a2 )                          

                          
parserFormulaList a0 =                     
  do 
    a1<- parserFormula (a0)
    a2<- parserFormulaList (a0)                    
    return (a1 : a2 )
  <|>
  do                     
    return ([] )                          

                          
parserFormula a0 =                     
  do 
    a1<- parserData (a0)
    a2<- parserSubAndSup (a0)                    
    return (Formula a1 (fst a2) (snd a2) )                          

                          
parserData a0 =                     
  do 
    a1<- parserSimpleWord (a0)                    
    return (SimpleWord a1 )                          

                          
parserSimpleWord a0 =                     
  do 
    a1<- token6 (a0)                    
    return (a1 )                          

                          
parserSubAndSup a0 =                     
  do 
    a1<- parserSub (a0)
    a2<- parserSup (a0)                    
    return ((a1, a2) )
  <|>
  do 
    a1<- parserSup (a0)
    a2<- parserSub (a0)                    
    return ((a2, a1) )
  <|>
  do 
    a1<- parserSub (a0)                    
    return ((a1, Nothing) )
  <|>
  do 
    a1<- parserSup (a0)                    
    return ((Nothing, a1) )
  <|>
  do                     
    return ((Nothing, Nothing) )                          

                          
parserSub a0 =                     
  do 
    a1<- token5 (a0)
    a2<- token1 (a0)
    a3<- parserFormulaList (a0)
    a4<- token2 (a0)                    
    return (Just (Formulas a3) )
  <|>
  do 
    a1<- token5 (a0)
    a2<- parserData (a0)                    
    return (Just (Formulas [(Formula a2 Nothing Nothing)]) )                          

                          
parserSup a0 =                     
  do 
    a1<- token4 (a0)
    a2<- token1 (a0)
    a3<- parserFormulaList (a0)
    a4<- token2 (a0)                    
    return (Just (Formulas a3) )
  <|>
  do 
    a1<- token4 (a0)
    a2<- parserData (a0)                    
    return (Just (Formulas [(Formula a2 Nothing Nothing)]) )                          

                                                              
                                                                
-- | Generated parser                                           
parser  x = run (_res <$> (parserStart x) <* pcEof)                           
                                                                
-- | After block                                                
token2word :: Token -> String
token2word (TokenWord x) = x

evaluateTokens :: [Token] -> Except ParseError Expressions
evaluateTokens = parser (Attributes $ Expressions [])

evaluate :: String -> Except ParseError Expressions
evaluate = evaluateTokens . tokenize





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

                                                              
