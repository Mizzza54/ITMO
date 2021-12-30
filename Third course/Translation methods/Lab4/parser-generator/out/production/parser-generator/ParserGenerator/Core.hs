module ParserGenerator.Core where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Applicative     (many, (<|>))
import           Control.Monad           (void)
import           Data.Char               (isAlpha, isAlphaNum)
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)
import qualified Data.Set                as Set
import           GHC.Natural             (Natural)
import           ParserGenerator.Base
import           Utils.Base              (Except (..), ParseError (..), Parser,
                                          modifyExceptState, run)
import           Utils.ParserCombinators

----------------------------------------------------------------------
-- Internal simple parsers
----------------------------------------------------------------------

parseHaskellCode :: Parser String String
parseHaskellCode = pcBetweenChar '{' '}' (pcTakeWhile (/= '}'))

parseLexerName :: Parser String String
parseLexerName = pcSatisfyElems "%lexername" >> parseHaskellCode

parseTokenName :: Parser String String
parseTokenName = pcSatisfyElems "%tokenname" >> parseHaskellCode

parseParserName :: Parser String String
parseParserName = pcSatisfyElems "%name" >> parseHaskellCode

parseAttributeDef :: Parser String String
parseAttributeDef = pcSkipSpaces $ do
  void $ pcSatisfyElems "%attributetype"
  res <- parseHaskellCode
  return res

parseDataAttribute :: Parser String DataAttribute
parseDataAttribute = pcSkipSpaces $ do
  def <- parseAttributeDef
  attr <- many parseAttribute
  return DataAttribute { definition = def, attributes = attr}

parseAttribute :: Parser String Attribute
parseAttribute = do
  void $ pcSatisfyElems "%attribute"
  name' <- pcSkipSpaces $ pcWord
  code' <- parseHaskellCode
  return Attribute { attributeName = name', attributeType = code'}

parseToken :: Parser String Token
parseToken = do
  pattern' <- pcWord
  constructor <- parseHaskellCode
  code' <- pcBetweenChar '%' '%' parseTokenCode
  return Token { pattern = pattern', tokenConstructor = constructor, tokenCode = safeHead $ arrayMaybe2array [code']}

parseTokenCode :: Parser String (Maybe String)
parseTokenCode =
  do
    res <- pcTakeWhile (/= '%')
    if (length res == 0)
      then do
        return Nothing
      else do
        return $ Just res
  <|>
  do
    return Nothing

parseTokens :: Parser String [Token]
parseTokens = pcSkipSpaces $ do
  void $ pcSkipSpaces $ pcSatisfyElems "%token"
  tokens' <- many parseToken
  void $ pcSatisfyElems "%%"
  return tokens'

parseParserRule :: Parser String TMPParser
parseParserRule = pcSkipSpaces $ do
  name' <- pcSkipSpaces $ many $ pcSatisfyPredicate isAlpha
  void $ pcSatisfyElem ':'
  options' <- many parseOption
  return TMPParser { name = name', tokenType = "", returnType = "", options = options' }

parseParserRules :: Parser String [TMPParser]
parseParserRules = do
  void $ pcSkipSpaces $ pcSatisfyElems "%rules"
  tokens' <- many parseParserRule
  void $ pcSatisfyElems "%%"
  return tokens'

parseOption :: Parser String ParserOption
parseOption = do
  context' <- parseContext
  code' <- pcBetweenChar '%' '%' (many (parseExpression))
  let proc = arrayMaybe2array code'
  let map' = Map.fromList proc
  let step = analyzeContext context' map'
  let expr = map (\x -> snd x) (filter (\x -> fst x == 0) proc)
  return ParserOption { variables = step, expression = safeHead expr, condition = ""}

parseExpression :: Parser String (Maybe (Int, String))
parseExpression =
  do
    num <- pcSkipSpaces $ pcRegExp "^\\$[0-9]+"
    void $ pcSkipSpaces $ pcSatisfyElems "::"
    res <- pcTakeWhile (\x -> x /= '%' && x /= ';')
    ((pcSatisfyElem ';')) <|> ((pcOk >> pure '%'))
    return $ Just (read $ tail num :: Int, res)
  <|>
  do
    pcFail
    return Nothing

parseContext :: Parser String [Context]
parseContext = do
  void $ pcSkipSpaces $ pcSatisfyElem '|'
  words' <- pcTakeWhile (/= '%')
  let split = words words'
  return $ map (\x -> Context { variable = x, context = [] }) split

----------------------------------------------------------------------
-- Main parser
----------------------------------------------------------------------

runParseGrammarFile :: Parser String CommonParser
runParseGrammarFile = do
  header <- parseHaskellCode
  attribute' <- parseDataAttribute
  lexerName' <- parseLexerName
  parserName' <- parseParserName
  tokenName' <- parseTokenName
  tokens' <- parseTokens
  rules' <- parseParserRules
  footer <- parseHaskellCode
  return CommonParser
    { doBefore = header
    , attribute = attribute'
    , lexerName = lexerName'
    , parserName = parserName'
    , tokenName = tokenName'
    , tokens = tokens'
    , tmpParsers = rules'
    , doAfter = footer
    }


----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

safeHead :: [String] -> String
safeHead [] = ""
safeHead x  = head x

arrayMaybe2array :: [Maybe a] -> [a]
arrayMaybe2array ((Just x) : xs) = x : (arrayMaybe2array xs)
arrayMaybe2array (Nothing : xs)  = arrayMaybe2array xs
arrayMaybe2array []              = []

analyzeContext :: [Context] -> Map.Map Int String -> [Context]
analyzeContext list mappa = map (\x -> action x) (zip [1 :: Int ..] list)
  where
  action x = case Map.lookup (fst x) mappa of
    Just r  -> Context { variable = variable (snd x), context = r}
    Nothing -> Context { variable = variable (snd x), context = "a0"}

--buildFirst :: [TMPParser] -> Map.Map String (Set.Set String)
--buildFirst x = helpAction (map name x)
--
--helpAction :: [String] -> Map.Map String (Set.Set String)
--helpAction list = foldl add Map.empty list where
--  add m x = Map.insert x Set.empty m
--
--helpAction2 :: [TMPParser] -> Bool -> Map.Map String (Set.Set String) -> Map.Map String (Set.Set String)
--helpAction2 x True mappa = undefined
--helpAction2 x False mappa = mappa
--
--helpAction4 :: [TMPParser] -> Map.Map String (Set.Set String) -> (Bool, Map.Map String (Set.Set String))
--helpAction4 list mappa = foldl action mappa list where
--  action m x = Set.union (fromMaybe (error ("core/helpaction4")) $ Map.lookup (name x) mappa) (Map.lookup ())
--
--
--data TempData = TempData
--  { name :: String
--
--  }
