{-# LANGUAGE DeriveDataTypeable  #-}
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

import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString                as BS
import qualified Data.Char                      as Char
import qualified Data.Map                       as Map
import qualified Data.Sequence                  as Seq
import qualified Data.Text                      as Text
import           Data.Text.Internal.Read        (hexDigitToInt)
import           Data.Void                      (Void)
import           Data.Word                      (Word8)
import           HW3.Base
import           Text.Megaparsec                (Parsec, between, choice, eof,
                                                 many, manyTill, notFollowedBy,
                                                 optional, parseTest, runParser,
                                                 satisfy, sepBy, try, (<|>))
import           Text.Megaparsec.Char           (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Error          (ParseErrorBundle)

import           Data.Char                      (isSpace)
import           Data.Data
import           Data.Maybe                     (fromJust)
import           Data.Typeable
import Data.Text (replace)
import Data.Char (isAlphaNum)

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

maybeParens :: Parser a -> Parser a
maybeParens parser = (between (symbol "(") (symbol ")") parser) <|> parser

pListSepByComma :: Parser a -> Parser [a]
pListSepByComma parser = parser `sepBy` symbol ","

----------------------------------------------------------------------
-- Parser for HiFun
----------------------------------------------------------------------

pHiFun :: Parser HiFun
pHiFun = lexeme . choice $ map pHiFunTemplate (allCtors :: [HiFun])

pHiFunTemplate :: HiFun -> Parser HiFun
pHiFunTemplate constructor = constructor <$ string (show constructor)

allCtors :: forall a. Data a => [a]
allCtors = map observeCtor $ dataTypeConstrs $ dataTypeOf (undefined :: a)
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
  , HiValueList     <$> pList
  , HiValueAction   <$> pCwd
  , HiValueAction   <$> pNow]

pRational :: Parser Rational
pRational = L.signed sc $ toRational <$> L.scientific

pBool :: Parser Bool
pBool = (string "true" >> return True) <|> (string "false" >> return False)

pNull :: Parser HiValue
pNull = string "null" >> return HiValueNull

pString :: Parser Text.Text
pString = Text.pack <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

pList :: Parser (Seq.Seq HiValue)
pList = Seq.fromList <$> between (symbol "[") (symbol "]") (pListSepByComma pHiValue)

pByte :: Parser Word8
pByte = lexeme $ do
  n1 <- hexDigitToInt <$> satisfy Char.isHexDigit
  n2 <- hexDigitToInt <$> satisfy Char.isHexDigit
  return $ fromInteger . toInteger $ n1 * 16 + n2

pBytes :: Parser BS.ByteString
pBytes = between (symbol "[#") (symbol "#]") (BS.pack <$> many pByte)

pCwd :: Parser HiAction
pCwd = string "cwd" >> return HiActionCwd

pNow :: Parser HiAction
pNow = string "now" >> return HiActionNow

pDotApply = undefined

----------------------------------------------------------------------
-- Simple parsers
----------------------------------------------------------------------

--pHiExprValue :: Parser HiExpr
--pHiExprValue = HiExprValue <$> pHiValue

pHiExprValue :: Parser HiExpr
pHiExprValue = HiExprValue
               <$> choice
  [ HiValueFunction <$> pHiFun
  , HiValueNumber   <$> pRational
  , HiValueBool     <$> pBool
  , pNull
  , HiValueString   <$> pString
  , HiValueBytes    <$> pBytes
  , HiValueList     <$> pList
  , HiValueAction   <$> pCwd
  , HiValueAction   <$> pNow]
  <|> parens pHiExpr

pHiExpr :: Parser HiExpr
pHiExpr = pMkOperators $ do
  expr <- pHiExprValue <|> pDict
  pHiExprApply expr

pHiExprApply :: HiExpr -> Parser HiExpr
pHiExprApply e = do
  res <- HiExprApply e <$> pHiExprApplyArgs
  pHiExprApply res
  <|>
  pure e

pHiExprApplyArgs :: Parser [HiExpr]
pHiExprApplyArgs = p1 <|> p2
  where
    p1 = parens $ pListSepByComma pHiExpr
    p2 = do
      _ <- symbol "."
      arg <- HiExprValue . HiValueString . (replace "-" " ") . Text.pack <$> many (satisfy (\x -> isAlphaNum x || x == '-'))
--      ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy` char '-'
      return [arg]

pDict :: Parser HiExpr
pDict = HiExprDict <$> between (symbol "{") (symbol "}") (pListSepByComma pKeyValue)

pKeyValue :: Parser (HiExpr, HiExpr)
pKeyValue = do
  key <- pHiExpr
  _ <- symbol ":"
  value <- pHiExpr
  return (key, value)

--pTerm :: Parser HiExpr
--pTerm = choice
--  [ parens (pMkOperators pTerm)
--  , pHiExpr
--  , maybeParens pDict
--  ]

----------------------------------------------------------------------
-- Operators parsers
----------------------------------------------------------------------

pMkOperators :: Parser HiExpr -> Parser HiExpr
pMkOperators a = makeExprParser a opTable

infixTemplate :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> String -> HiFun -> Operator Parser HiExpr
infixTemplate operator name hiFun = operator $ (\l r -> (HiExprApply . HiExprValue . HiValueFunction) hiFun [l, r]) <$ symbol name

infixLeft :: String -> HiFun -> Operator Parser HiExpr
infixLeft = infixTemplate InfixL

infixNon :: String -> HiFun -> Operator Parser HiExpr
infixNon = infixTemplate InfixN

infixRight :: String -> HiFun -> Operator Parser HiExpr
infixRight = infixTemplate InfixR

postfix :: String -> (HiExpr -> HiExpr) -> Operator Parser HiExpr
postfix name f = Postfix (f <$ symbol name)

opTable :: [[Operator Parser HiExpr]]
opTable =
  [ [ postfix "!" HiExprRun]
  , [ InfixL $ (\l r -> (HiExprApply . HiExprValue . HiValueFunction) HiFunDiv [l, r]) <$ (lexeme . try) (string "/" <* notFollowedBy (char '='))
    , infixLeft "*" HiFunMul
    ]
  , [ infixLeft "+" HiFunAdd
    , infixLeft "-" HiFunSub
    ]
  , [ infixNon "<" HiFunLessThan
    , infixNon ">" HiFunGreaterThan
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
