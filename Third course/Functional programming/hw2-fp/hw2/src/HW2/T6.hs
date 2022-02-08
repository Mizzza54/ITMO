{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module HW2.T6
  ( ParseError (..)
  , Parser (..)
  , lexeme
  , pAddAndSub 
  , pBetweenParens
  , pChar
  , pCloseParen
  , pConst
  , pDigit
  , pDouble
  , pEof
  , pExpr
  , pExpr'
  , pFactor
  , pMathExpression
  , pMulAndDiv
  , pOpenParen
  , pParensExpr
  , pSpace
  , pTerm
  , pTerm'
  , parseError
  , parseExpr
  , runP
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Control.Applicative (Alternative, empty, many, some, (<**>), (<|>))
import Control.Monad.Cont (MonadPlus, mfilter, void)

import Data.Char
import Data.Functor (($>))
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Sequence

import GHC.Natural (Natural)

import HW2.T1 (Annotated (..), Except (..))
import HW2.T4 (Expr (..), Prim (..))
import HW2.T5 (ExceptState (..), runES)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Alternative Parser where
  empty = parseError

  (<|>) (P innerPA) (P innerPB) = P $ ES \(pos, s) ->
    case runES innerPA (pos, s) of
      Success (a :# s') -> Success (a :# s')
      Error _ -> case runES innerPB (pos, s) of
        Success (b :# s') -> Success (b :# s')
        Error e           -> Error e

instance MonadPlus Parser   -- No methods.

----------------------------------------------------------------------
-- Basic parsers and utilities
----------------------------------------------------------------------

runP :: Parser a -> String -> Except ParseError a
runP (P inner) t = case runES inner (0, t) of
  Success (a :# _) -> Success a
  Error e          -> Error e

-- | This parser that consumes a single character
-- 
-- If you have the following questions:
--     1. What happens when the string is empty?
--     2. How does the parser state change when a character is consumed? 
-- then I will answer them like this:
--     1. This parser throw error because "" == [] == True.
--     2. After the successful consumption of the symbol, 
--        the state of the parser will have the following form: 
--        Success ('с' :# (pos + 1,"cs")), where 'c' - is consumed char,
--        pos - old position, "cs" - the remainder of the input string.
pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES \(pos, _) ->
  Error (ErrorAtPos pos)

pEof :: Parser ()
pEof = P $ ES \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, s))
    _  -> Error (ErrorAtPos pos)

pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy p = mfilter p pChar

pDigit :: Parser Integer
pDigit =  toInteger . digitToInt <$> pSatisfy isDigit

pDouble :: Parser Double
pDouble = do
  p1 <- Sequence.fromList <$> (some $ pDigit)
  void $ (:[]) <$> pSatisfy (== '.')
  p2 <- Sequence.fromList <$> (some $ pDigit)
  let number = foldl (\acc x -> (acc * 10) + x) 0 (p1 Sequence.>< p2)
  let fractionalLength = Sequence.length p2
  pure $ Scientific.toRealFloat . Scientific.scientific number $ negate fractionalLength

pSpace :: Parser ()
pSpace = void $ many $ pSatisfy isSpace

lexeme :: Parser a -> Parser a
lexeme p = do
  void pSpace
  res <- p
  void pSpace
  pure res

pOpenParen :: Parser Char
pOpenParen = lexeme $ pSatisfy (== '(')

pCloseParen :: Parser Char
pCloseParen = lexeme $ pSatisfy (== ')')

pBetweenParens :: Parser a -> Parser a
pBetweenParens p = do
  void pOpenParen
  res <- p
  void pCloseParen
  pure res

----------------------------------------------------------------------
-- Mathematical expression parser
----------------------------------------------------------------------

-- | This Mathematical expression parser handle floating-point
-- literals of the form 4.09, the operators + - * / with the usual precedence
-- (multiplication and division bind tighter than addition and subtraction),
-- and parentheses.
--
-- This grammar describes mathematical expressions
-- and this parser was built on the basis of it.
--
--     1. @expr        ::= term expr’@
--     2. @expr’       ::= (add/sub)Op term expr’ | empty@
--     3. @term        ::= factor term’@
--     4. @term’       ::= (mul/div)Op factor term’ | empty@
--     5. @factor      ::= const | "(" expr ")"@
--     6. @(add/sub)Op ::= "+" | "-"@
--     7. @(mul/div)Op ::= "*" | "/"@
--
-- Example usage:
--
--     > > parseExpr "3.14 + 1.618 * 2"
--     > Success (Op (Add (Val 3.14) (Op (Mul (Val 1.618) (Val 2.0)))))
--
--     > > parseExpr "2 * (1 + 3)"
--     > Success (Op (Mul (Val 2.0) (Op (Add (Val 1.0) (Val 3.0)))))
--
--     > > parseExpr "24 + Hello"
--     > Error (ErrorAtPos 3)
parseExpr :: String -> Except ParseError Expr
parseExpr = runP pMathExpression

pConst :: Parser Expr
pConst = lexeme $ Val <$> pDouble

pParensExpr :: Parser Expr
pParensExpr = pBetweenParens pExpr

pAddAndSub :: Parser (a -> a -> Prim a)
pAddAndSub = pPlus <|> pMinus
  where
    pPlus = pSatisfy (== '+') $> Add
    pMinus = pSatisfy (== '-') $> Sub

pMulAndDiv :: Parser (a -> a -> Prim a)
pMulAndDiv = pMul <|> pDiv
  where
    pMul = pSatisfy (== '*') $> Mul
    pDiv = pSatisfy (== '/') $> Div

pFactor :: Parser Expr
pFactor = pConst <|> pParensExpr

pTerm' :: Expr -> Parser Expr
pTerm' e = (pure e <**> pMulAndDiv <*> pFactor >>= pTerm' . Op) <|> return e

pTerm :: Parser Expr
pTerm = pFactor >>= pTerm'

pExpr' :: Expr -> Parser Expr
pExpr' e = (pure e <**> pAddAndSub <*> pTerm >>= pExpr' . Op) <|> return e

pExpr :: Parser Expr
pExpr = pTerm >>= pExpr'

pMathExpression :: Parser Expr
pMathExpression = pExpr <* pEof
