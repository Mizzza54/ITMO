module ParserGenerator.Base
  ( Token (..)
  , Attribute (..)
  , Attributes (..)
  , Context (..)
  , Option (..)
  , Grammar (..)
  , CommonParser (..)
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Data.List   (elemIndex, intercalate)
import           Text.Printf (printf)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

data Token = Token
  { tokenPattern     :: String
  , tokenConstructor :: String
  , tokenCode        :: String
  }

data Attribute = Attribute
  { attributeName :: String
  , attributeType :: String
  }

data Attributes = Attributes
  { attributesDefinition :: String
  , attributes           :: [Attribute]
  }

data Context = Context
  { contextVariable :: String
  , context         :: String
  }

data Option = Option
  { optionVariables  :: [Context]
  , optionExpression :: String
  , optionFirst      :: [Token]
  , optionFollow     :: [Token]
  }

data Grammar = Grammar
  { grammarNameFrom :: String
  , grammarOptions  :: [Option]
  }

data CommonParser = CommonParser
  { haskellCodeHeader :: String
  , parserAttributes  :: Attributes
  , parserName        :: String
  , parserTokens      :: [Token]
  , parserGrammar     :: [Grammar]
  , haskellCodeFooter :: String
  }

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Show Token where
  show x = printf (unlines templateToken)
    (tokenCode x)
    (tokenConstructor x)

instance Show Attribute where
  show x = "_" ++ attributeName x ++ " :: " ++ attributeType x

instance Show Attributes where
  show x = printf (unlines templateAttributes)
    (attributesDefinition x)
    (genericJoin "\n  , " (attributes x))

instance Show Context where
  show x = "<- " ++ contextVariable x ++ " (" ++ context x ++ ")"

instance Show Option where
  show x = printf (unlines templateOption)
    (tokenConstructor $ head $ optionFirst x)
    ("\n      " ++ intercalate "\n      " (zipWith (\a b -> 'a' : show a ++ show b) [1..] (optionVariables x)))
    (optionExpression x)

instance Show Grammar where
  show token = printf (unlines templateGrammar)
    (toTemplateParserName (grammarNameFrom token))
    (genericJoin "\n    " (grammarOptions token))

instance Show CommonParser where
  show x = printf (unlines templateCommonParser)
    (haskellCodeHeader x)
    (show (parserAttributes x))
    (intercalate "\n" (zipWith (\a b -> toTemplateTokenName a ++ show b) [1..] (parserTokens x)))
    (unlines $ map show (print2 x))
    (parserName x)
    (toTemplateParserName (grammarNameFrom $ head $ parserGrammar x))
    (haskellCodeFooter x)

----------------------------------------------------------------------
-- Templates
----------------------------------------------------------------------

templateCommonParser :: [String]
templateCommonParser =
  [ "{-# LANGUAGE BlockArguments #-}"
  , "%s"
  , ""
  , "import           Control.Applicative       ((<|>))"
  , "import           Utils.Base                (Annotated (..), Except (..),"
  , "                                            ExceptState (..), ParseError (..),"
  , "                                            Parser, modifyExceptState, run)"
  , "import           Utils.ParserCombinators   (pcEof, pcSatisfyPredicate)"
  , ""
  , "%s"
  , ""
  , "%s"
  , ""
  , "%s"
  , ""
  , "%s x = run (_res <$> (%s x) <* waitTokenEndInput)"
  , ""
  , "nextToken :: Parser [Token] Token"
  , "nextToken = ES \\(pos, s) -> case s of"
  , "  []     -> Success (TokenEndInput :# (pos, []))"
  , "  (c:cs) -> Success (c :# (pos + 1, cs))"
  , ""
  , "rollback Epsilon = id"
  , "rollback foo = \\x -> (fst x - 1, foo : snd x)"
  , ""
  , "waitTokenEndInput :: Parser [Token] ()"
  , "waitTokenEndInput = ES \\(pos, s) ->"
  , "  case s of"
  , "    [TokenEndInput] -> Success (() :# (pos, []))"
  , "    []              -> Success (() :# (pos, []))"
  , "    _               -> Error (ErrorAtPos pos)"
  , ""
  , "%s"]

templateAttributes :: [String]
templateAttributes =
  [ "data %s = Attributes"
  , "  { %s"
  , "  }"]

templateToken :: [String]
templateToken =
  [ " a0 = do"
  , "  a1 <- pcSatisfyPredicate func"
  , "  return (%s)"
  , "  where func (%s) = True"
  , "        func _ = False"]

templateOption :: [String]
templateOption =
  [ "%s -> do %s"
  , "      return (%s)"]

templateGrammar :: [String]
templateGrammar =
  [ "%s a0 = do"
  , "  foo <- nextToken"
  , "  modifyExceptState (rollback foo)"
  , "  case foo of"
  , "    %s"]

toTemplateParserName :: String -> String
toTemplateParserName x = "parser" ++ x

toTemplateTokenName :: Int -> String
toTemplateTokenName x = "token" ++ show x

----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

help1 :: CommonParser -> [String]
help1 parser = map tokenPattern (parserTokens parser)

print2 :: CommonParser -> [Grammar]
print2 parser = map (\x -> Grammar { grammarNameFrom = grammarNameFrom x, grammarOptions = map tempFunc (grammarOptions x)}) (parserGrammar parser) where
  tempFunc y = Option { optionVariables = newvar y, optionExpression = optionExpression y, optionFirst = optionFirst y, optionFollow = optionFollow y}
  newvar y = map (find (help1 parser) ) (optionVariables y)

find :: [String] -> Context -> Context
find token cotex = case contextVariable cotex `elemIndex` token of
  Just n  -> Context { contextVariable = toTemplateTokenName (n + 1), context = context cotex}
  Nothing -> Context { contextVariable = toTemplateParserName $ contextVariable cotex, context = context cotex}



-- | Combines objects to @String@ via a delimeter
genericJoin :: Show a => String -> [a] -> String
genericJoin delimit l = intercalate delimit (map show l)
