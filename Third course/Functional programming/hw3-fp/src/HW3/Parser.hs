{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HW3.Parser
  ( parse
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isAlphaNum, isHexDigit, isSpace)

import Data.Data (Constr, Data, cast, dataTypeConstrs, dataTypeOf, fromConstrM)
import Data.Functor (void)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Internal.Read (hexDigitToInt)
import Data.Void (Void)
import Data.Word (Word8)


import Text.Megaparsec (Parsec, between, choice, eof, many, manyTill, notFollowedBy, runParser,
                        satisfy, sepBy, sepBy1, sepEndBy, try, (<|>))
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)

import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

type Parser = Parsec Void String

----------------------------------------------------------------------
-- Internal parser utilities
----------------------------------------------------------------------

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pListSepByComma :: Parser a -> Parser [a]
pListSepByComma parser = parser `sepBy` symbol ","

----------------------------------------------------------------------
-- Parser for HiFun
----------------------------------------------------------------------

pHiFun :: Parser HiFun
pHiFun = lexeme . choice $ map pHiFunTemplate (allConstructors :: [HiFun])

pHiFunTemplate :: HiFun -> Parser HiFun
pHiFunTemplate constructor = constructor <$ string (show constructor)

allConstructors :: forall a. Data a => [a]
allConstructors = map observeCtor $ dataTypeConstrs $ dataTypeOf (undefined :: a)
  where
    observeCtor :: Constr -> a
    observeCtor c = fromJust $ fromConstrM (cast c) c

----------------------------------------------------------------------
-- Parser for HiValue
----------------------------------------------------------------------

pHiValue :: Parser HiValue
pHiValue = lexeme $ choice
  [ HiValueFunction <$> pHiFun
  , HiValueNumber   <$> pRational
  , HiValueBool     <$> pBool
  , pNull
  , HiValueString   <$> pString
  , HiValueBytes    <$> pBytes
  , HiValueAction   <$> pCwd
  , HiValueAction   <$> pNow]

pRational :: Parser Rational
pRational = L.signed sc $ toRational <$> L.scientific

pBool :: Parser Bool
pBool = (string "true" >> return True) <|> (string "false" >> return False)

pNull :: Parser HiValue
pNull = string "null" >> return HiValueNull

pString :: Parser Text
pString = Text.pack <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

pList :: Parser HiExpr
pList = HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$> between (symbol "[") (symbol "]") (pListSepByComma pHiExpr)

pByte :: Parser Word8
pByte = do
  n1 <- hexDigitToInt <$> satisfy isHexDigit
  n2 <- hexDigitToInt <$> satisfy isHexDigit
  return $ fromInteger . toInteger $ n1 * 16 + n2

pBytes :: Parser ByteString
pBytes = between (symbol "[#") (symbol "#]") (BS.pack <$> pByte `sepEndBy` symbol " ")

pCwd :: Parser HiAction
pCwd = string "cwd" >> return HiActionCwd

pNow :: Parser HiAction
pNow = string "now" >> return HiActionNow

----------------------------------------------------------------------
-- Simple parsers
----------------------------------------------------------------------

pHiExprValue :: Parser HiExpr
pHiExprValue = HiExprValue <$> pHiValue <|> parens pHiExpr <|> pList

pHiExpr :: Parser HiExpr
pHiExpr = pMkOperators $ lexeme $ do
  expr <- pHiExprValue <|> pDict
  pHiExprApply expr

pHiExprApply :: HiExpr -> Parser HiExpr
pHiExprApply e = do
  res <- HiExprApply e <$> pHiExprApplyArgs
  pHiExprApply res
  <|> do
  void $ satisfy (== '!')
  pHiExprApply (HiExprRun e)
  <|>
  pure e

pHiExprApplyArgs :: Parser [HiExpr]
pHiExprApplyArgs = p1 <|> p2
  where
    p1 = parens $ pListSepByComma pHiExpr
    p2 = do
      _ <- char '.'
      arg <- HiExprValue . HiValueString . Text.pack . intercalate "-" <$> 
        ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
      return [arg]

pDict :: Parser HiExpr
pDict = HiExprDict <$> between (symbol "{") (symbol "}") (pListSepByComma pKeyValue)

pKeyValue :: Parser (HiExpr, HiExpr)
pKeyValue = do
  key <- pHiExpr
  void $ symbol ":"
  value <- pHiExpr
  return (key, value)

----------------------------------------------------------------------
-- Operators parsers
----------------------------------------------------------------------

pMkOperators :: Parser HiExpr -> Parser HiExpr
pMkOperators a = makeExprParser a opTable

infixTemplate 
  :: (Parser (HiExpr -> HiExpr -> HiExpr) 
  -> Operator Parser HiExpr) 
  -> String 
  -> String 
  -> HiFun 
  -> Operator Parser HiExpr
infixTemplate operator notFollow name hiFun = 
  let header parser = operator $ (\l r -> (HiExprApply . HiExprValue . HiValueFunction) hiFun [l, r]) <$ parser in
    header $ case notFollow of
      ""  -> (lexeme . try) $ (string name)
      str -> (lexeme . try) (string name <* notFollowedBy (string str))

infixLeft :: String -> HiFun -> Operator Parser HiExpr
infixLeft = infixTemplate InfixL ""

infixNon :: String -> HiFun -> Operator Parser HiExpr
infixNon = infixTemplate InfixN ""

infixRight :: String -> HiFun -> Operator Parser HiExpr
infixRight = infixTemplate InfixR ""

opTable :: [[Operator Parser HiExpr]]
opTable =
  [
    [ infixTemplate InfixL "=" "/" HiFunDiv
    , infixLeft "*" HiFunMul
    ]
  , [ infixLeft "+" HiFunAdd
    , infixLeft "-" HiFunSub
    ]
  , [ infixTemplate InfixN "=" "<" HiFunLessThan
    , infixTemplate InfixN "=" ">" HiFunGreaterThan
    , infixNon "<=" HiFunNotGreaterThan
    , infixNon ">=" HiFunNotLessThan
    , infixNon "==" HiFunEquals
    , infixNon "/=" HiFunNotEquals
    ]
  , [ infixRight "&&" HiFunAnd
    ]
  , [ infixRight "||" HiFunOr
    ]
  ]

----------------------------------------------------------------------
-- Parser runner
----------------------------------------------------------------------

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (pHiExpr <* eof) (Text.unpack "../logParser.log") . dropWhile isSpace
