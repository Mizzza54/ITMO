                                                                
module GeneratedKotlinArray.Parser (evaluate) where

import GeneratedKotlinArray.Lexer (Token(..), tokenize)
                                                              
                                                                
import           Control.Applicative     ((<|>))                
import           Utils.Base              (Except (..), ParseError (..), Parser,   
                                          run)                                    
import           Utils.ParserCombinators (pcSatisfyPredicate, pcEof)   
                                                                
data Data  = Attributes      
  { _res :: String                     
  }                       
                          
                                                              
                                                                
token1 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (a0 )                      
    where func (TokenVar ) = True          
          func _ = False
token2 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (a0 )                      
    where func (TokenArray ) = True          
          func _ = False
token3 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (a0 )                      
    where func (TokenOC ) = True          
          func _ = False
token4 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (a0 )                      
    where func (TokenCC ) = True          
          func _ = False
token5 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (a0 )                      
    where func (TokenSemicolon ) = True          
          func _ = False
token6 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (a0 )                      
    where func (TokenColon ) = True          
          func _ = False
token7 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (a0 )                      
    where func (TokenComma ) = True          
          func _ = False
token8 a0 =                               
  do                                  
    a1 <- pcSatisfyPredicate func      
    return (Attributes { _res = (token2word a1) } )                      
    where func (TokenWord _ ) = True          
          func _ = False                                                              
                                                                
-- | Generated parsers                                          
                                                                
                          
parserD a0 =                     
  do 
    a1<- token1 (a0)
    a2<- token8 (a0)
    a3<- token6 (a0)
    a4<- token2 (a0)
    a5<- token3 (a0)
    a6<- parserT (a0)
    a7<- token4 (a0)
    a8<- token5 (a0)                    
    return (a0 )                          

                          
parserT a0 =                     
  do 
    a1<- token8 (a0)
    a2<- parserTT (a0)                    
    return (0 )
  <|>
  do 
    a1<- token2 (a0)
    a2<- parserTT (a0)                    
    return (0 )                          

                          
parserTT a0 =                     
  do 
    a1<- token3 (a0)
    a2<- parserT (a0)
    a3<- token4 (a0)                    
    return (a0 )
  <|>
  do 
    a1<- token7 (a0)
    a2<- parserT (a0)                    
    return (a0 )
  <|>
  do 
    a1<- token3 (a0)
    a2<- parserT (a0)
    a3<- token4 (a0)
    a4<- token7 (a0)
    a5<- parserT (a0)                    
    return (a0 )
  <|>
  do                     
    return (a0 )                          

                                                              
                                                                
-- | Generated parser                                           
parser  x = run (_res <$> (parserD x) <* pcEof)                           
                                                                
-- | After block                                                
token2word :: Token -> String
token2word (TokenWord x) = x

evaluateTokens :: [Token] -> Except ParseError String
evaluateTokens = parser (Attributes "")

evaluate :: String -> Except ParseError String
evaluate = evaluateTokens . tokenize

                                                              
