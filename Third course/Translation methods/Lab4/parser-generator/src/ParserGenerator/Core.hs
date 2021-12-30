module ParserGenerator.Core where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Applicative              (many, (<|>))
import           Control.Monad                    (void)
import           Data.Char                        (isAlpha)
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import           ParserGenerator.Base
import           ParserGenerator.FirstFollowBuild
import           Utils.Base                       (Parser)
import           Utils.ParserCombinators

----------------------------------------------------------------------
-- Internal simple parsers
----------------------------------------------------------------------

parseHaskellCode :: Parser String String
parseHaskellCode = pcBetweenChar '{' '}' (pcTakeWhile (/= '}'))

parseParserName :: Parser String String
parseParserName = pcSatisfyElems "%name" >> parseHaskellCode

parseAttributeDef :: Parser String String
parseAttributeDef = pcSkipSpaces $ do
  void $ pcSatisfyElems "%attributetype"
  parseHaskellCode

parseDataAttribute :: Parser String Attributes
parseDataAttribute = pcSkipSpaces $ do
  def <- parseAttributeDef
  attr <- many parseAttribute
  return Attributes { attributesDefinition = def, attributes = attr}

parseAttribute :: Parser String Attribute
parseAttribute = do
  void $ pcSatisfyElems "%attribute"
  name' <- pcSkipSpaces pcWord
  code' <- parseHaskellCode
  return Attribute { attributeName = name', attributeType = code'}

parseToken :: Parser String Token
parseToken = do
  pattern' <- pcWord
  constructor <- parseHaskellCode
  code' <- pcBetweenChar '%' '%' parseTokenCode
  return Token { tokenPattern = pattern', tokenConstructor = constructor, tokenCode = safeHead $ arrayMaybe2array [code']}

parseTokenCode :: Parser String (Maybe String)
parseTokenCode =
  do
    res <- pcTakeWhile (/= '%')
    if null res
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
  return $ Token { tokenPattern = "TokenEndInput", tokenConstructor = "TokenEndInput", tokenCode = ""} : tokens'

parseParserRule :: Parser String Grammar
parseParserRule = pcSkipSpaces $ do
  name' <- pcSkipSpaces $ many $ pcSatisfyPredicate isAlpha
  void $ pcSatisfyElem ':'
  options' <- many parseOption
  return Grammar { grammarNameFrom = name', grammarOptions = options' }

parseParserRules :: Parser String [Grammar]
parseParserRules = do
  void $ pcSkipSpaces $ pcSatisfyElems "%rules"
  tokens' <- many parseParserRule
  void $ pcSatisfyElems "%%"
  return tokens'

parseOption :: Parser String Option
parseOption = do
  context' <- parseContext
  code' <- pcBetweenChar '%' '%' (many parseExpression)
  let proc = arrayMaybe2array code'
  let map' = Map.fromList proc
  let step = analyzeContext context' map'
  let expr = map (\x -> snd x) (filter (\x -> fst x == 0) proc)
  return Option { optionVariables = step, optionExpression = safeHead expr, optionFirst = [], optionFollow = []}

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
  return $ map (\x -> Context { contextVariable = x, context = [] }) split

----------------------------------------------------------------------
-- Main parser
----------------------------------------------------------------------

runParseGrammarFile :: Parser String CommonParser
runParseGrammarFile = do
  header <- parseHaskellCode
  attribute' <- parseDataAttribute
  parserName' <- parseParserName
  tokens' <- parseTokens
  rules' <- parseParserRules
  footer <- parseHaskellCode
  newRules <- addFirstAndFollowToOption tokens' rules'
  return CommonParser
    { haskellCodeHeader = header
    , parserAttributes = attribute'
    , parserName = parserName'
    , parserTokens = tokens'
    , parserGrammar = newRules
    , haskellCodeFooter = footer
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
    Just r  -> Context { contextVariable = contextVariable (snd x), context = r}
    Nothing -> Context { contextVariable = contextVariable (snd x), context = "a0"}

prepareTokenData :: [Token] -> Map.Map String Atom
prepareTokenData l = Map.fromList $ map (\x -> (tokenPattern x, Terminal (tokenPattern x))) l

prepareTMPParserData :: [Grammar] -> Map.Map String Atom
prepareTMPParserData l = Map.fromList $ map (\x -> (grammarNameFrom x, NotTerminal (grammarNameFrom x))) l

prepareAtomData :: [Token] -> [Grammar] -> [Atom]
prepareAtomData listToken listTMPParser = map (\x -> Terminal (tokenPattern x)) listToken ++ map (\x -> NotTerminal (grammarNameFrom x)) listTMPParser

prepareRules :: Map.Map String Atom -> Map.Map String Atom -> [Grammar] -> [Rule]
prepareRules termMap ntermMap list = foldl (++) [] $ map (\x -> action x) list where
  action x = map (\y -> Rule { from = (returnAtomByName termMap ntermMap $ grammarNameFrom x), to = help y}) (grammarOptions x)
  help y = if null (optionVariables y)
  then
    [Epsilon]
  else
    map (returnAtomByName termMap ntermMap . contextVariable) (optionVariables y)

returnAtomByName :: Map.Map String Atom -> Map.Map String Atom -> String -> Atom
returnAtomByName termMap ntermMap str = case Map.lookup str termMap of
  Just x -> x
  Nothing -> case Map.lookup str ntermMap of
    Just x  -> x
    Nothing -> Epsilon


addFirstAndFollowToOption :: Monad m => [Token] -> [Grammar] -> m [Grammar]
addFirstAndFollowToOption tokenList tmpParserList = do
  let tokenMap = Map.fromList $ map (\x -> (tokenPattern x, x)) tokenList
  let termMap = prepareTokenData tokenList
  let ntermMap = prepareTMPParserData tmpParserList
  let atomData = prepareAtomData tokenList tmpParserList
  let rules = prepareRules termMap ntermMap tmpParserList
  let startName = grammarNameFrom $ head tmpParserList
  let setFirst = buildFirst True rules (mappa $ atomData)
  let setFollow = buildFollow True rules setFirst (Map.insert (returnAtomByName termMap ntermMap startName) (Set.singleton End) (mappa (filter (\x -> isNTerminal x) atomData)))
  let newOptions = updateTmpParsers tokenMap termMap ntermMap tmpParserList setFirst setFollow
  return newOptions

updateTmpParsers :: Map.Map String Token -> Map.Map String Atom -> Map.Map String Atom ->[Grammar] -> Map.Map Atom (Set.Set Atom) -> Map.Map Atom (Set.Set Atom) -> [Grammar]
updateTmpParsers tokenMap termMap ntermMap tmpParsersList first follow =
  map
  (\x -> Grammar
    { grammarNameFrom = grammarNameFrom x,
    grammarOptions = update $ updateOptions tokenMap termMap ntermMap (grammarNameFrom x) first follow (grammarOptions x)})
  tmpParsersList

update :: [Option] -> [Option]
update options = foldl (\x y -> x ++ (printCasePattern y)) [] options

printCasePattern :: Option -> [Option]
printCasePattern opt = if null $ optionFirst opt
then
  map (\x -> Option { optionVariables = optionVariables opt, optionExpression = optionExpression opt, optionFirst = [x], optionFollow = []}) (optionFollow opt)
else
  if (length $ optionFirst opt) > 1
  then
    map (\x -> Option { optionVariables = optionVariables opt, optionExpression = optionExpression opt, optionFirst = [x], optionFollow = []}) (optionFirst opt)
  else
    [opt]

updateOptions :: Map.Map String Token -> Map.Map String Atom -> Map.Map String Atom -> String -> Map.Map Atom (Set.Set Atom) -> Map.Map Atom (Set.Set Atom) -> [Option] -> [Option]
updateOptions tokenMap termMap ntermMap from first follow options = map (updateOption tokenMap termMap ntermMap from first follow) options

updateOption :: Map.Map String Token -> Map.Map String Atom -> Map.Map String Atom -> String -> Map.Map Atom (Set.Set Atom) -> Map.Map Atom (Set.Set Atom) -> Option -> Option
updateOption tokenMap termMap ntermMap from first follow opt = if Set.member Epsilon (getFirst first [returnAtomByName termMap ntermMap from])
  then
    Option { optionVariables = optionVariables opt, optionExpression = optionExpression opt, optionFirst = atoms2Tokens tokenMap $ getFirst first (variables2atoms termMap ntermMap (optionVariables opt)), optionFollow = atoms2Tokens tokenMap $ (Map.!) follow (returnAtomByName termMap ntermMap from)}
  else
    Option { optionVariables = optionVariables opt, optionExpression = optionExpression opt, optionFirst = atoms2Tokens tokenMap $ getFirst first (variables2atoms termMap ntermMap (optionVariables opt)), optionFollow = []}

variables2atoms :: Map.Map String Atom -> Map.Map String Atom -> [Context] -> [Atom]
variables2atoms termMap ntermMap = foldr (\x y -> (returnAtomByName termMap ntermMap $ contextVariable x) : y) []


atom2Token :: Atom -> Map.Map String Token -> Token
atom2Token (Terminal x) tokenMap = (Map.!) tokenMap x
atom2Token End tokenMap          = (Map.!) tokenMap "TokenEndInput"
atom2Token Epsilon tokenMap      = Token { tokenPattern = "Epsilon", tokenConstructor = "Epsilon", tokenCode = ""}
atom2Token s _                   = error "atom2token -> unexpected"

atoms2Tokens :: Map.Map String Token -> Set.Set Atom -> [Token]
atoms2Tokens tokenMap atoms = map (\x -> atom2Token x tokenMap) (Set.elems atoms)
