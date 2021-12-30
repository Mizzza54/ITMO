module ParserGenerator.Base where

import           Data.Char       (isSpace)
import           Data.List       (elemIndex, intercalate)
import           Text.Printf     (printf)
import           Text.Regex.TDFA ((=~))

data Token = Token
  { pattern          :: String
  , tokenConstructor :: String
  , tokenCode        :: String
  }

data Attribute = Attribute
  { attributeName :: String
  , attributeType :: String
  }

data DataAttribute = DataAttribute
  { definition :: String
  , attributes :: [Attribute]
  }

data Context = Context
  { variable :: String
  , context  :: String
  }

data ParserOption = ParserOption
  { variables  :: [Context]
  , expression :: String
  , condition  :: String
  }

data TMPParser = TMPParser
  { name       :: String
  , tokenType  :: String
  , returnType :: String
  , options    :: [ParserOption]
  }

data CommonParser = CommonParser
  { doBefore   :: String
  , attribute  :: DataAttribute
  , lexerName  :: String
  , parserName :: String
  , tokenName  :: String
  , tokens     :: [Token]
  , tmpParsers :: [TMPParser]
  , doAfter    :: String
  }


toTmpName :: String -> String
toTmpName x = "parser" ++ x

toTmpTokenName :: Int -> String
toTmpTokenName x = "token" ++ show x

tmpCurrent :: String
tmpCurrent = "a0"

exist :: String -> String
exist []  = []
exist str = "\n    " ++ str

instance Show Context where
  show contx =
    printf
      "<- %s (%s)"
      (variable contx)
      ( (context contx))

instance Show Attribute where
  show attribut =
    "_" ++ (attributeName attribut)
      ++ " :: "
      ++ (attributeType attribut)

instance Show DataAttribute where
  show atr =
    printf
      "data %s = Attributes      \n\
      \  { %s                    \n\
      \  }                       \n\
      \                          \n"
      (definition atr)
      (genericJoin "\n  , " (attributes atr))

replaceChild :: String -> String
replaceChild = replace "\\$[0-9]+" (\_ -> "a")

instance Show ParserOption where
  show opt =
    printf
      "  do %s%s                    \n\
      \    return (%s)"
--      ( exist
--          ( join
--              "\n    "
--              ( zip [1 :: Int ..] (opt ^. variables)
--                  <&> _1 %~ (('a' :) . show)
--                  <&> _2 %~ show
--                  <&> (^. each)
--              )
--          )
--      )
      (exist (join "\n    " (map (\x -> (('a' : (show (fst x))) ++ (show $ snd x))) (zip [1 :: Int ..] (variables opt)))))
      (exist (replaceChild (condition opt)))
--      (printExpression (expression opt))
      (expression opt)

createDefinition :: String -> String -> String -> String
createDefinition _ _ [] = []
createDefinition defName tokType retType =
  printf
    "%s :: Parser [%s] %s"
    defName
    tokType
    retType

instance Show Token where
  show token =
    printf
      " %s =                               \n\
      \  do                                  \n\
      \    a1 <- pcSatisfyPredicate func      \n\
      \    return (%s)                      \n\
      \    where func (%s) = True          \n\
      \          func _ = False"
      tmpCurrent
--      (printExpression (tokenCode token))
      (tokenCode token)
      (tokenConstructor token)

instance Show TMPParser where
  show token =
    printf
      "%s                          \n\
      \%s %s =                     \n\
      \%s                          \n"
      ( createDefinition
          (toTmpName (name token))
          (tokenType token)
          (returnType token)
      )
      (toTmpName (name token))
      tmpCurrent
      (genericJoin "\n  <|>\n" (options token))

instance Show CommonParser where
  show parser =
    printf
      "                                                                \n\
      \%s                                                              \n\
      \                                                                \n\
      \import           Control.Applicative     ((<|>))                \n\
      \import           Utils.Base              (Except (..), ParseError (..), Parser,   \n\
      \                                          run)                                    \n\
      \import           Utils.ParserCombinators (pcSatisfyPredicate, pcEof)   \n\
      \                                                                \n\
      \%s                                                              \n\
      \                                                                \n\
      \%s                                                              \n\
      \                                                                \n\
      \-- | Generated parsers                                          \n\
      \                                                                \n\
      \%s                                                              \n\
      \                                                                \n\
      \-- | Generated parser                                           \n\
      \%s x = run (_res <$> (%s x) <* pcEof)                           \n\
      \                                                                \n\
      \-- | After block                                                \n\
      \%s                                                              \n"
      (doBefore parser)
      (show (attribute parser))
      ( join "\n" (print1 parser))   -- ( zip [1 :: Int ..] (tokens parser) <&> _1 %~ toTmpTokenName <&> _2 %~ show <&> (^. each))
      (unlines $ map show (print2 parser))
--      ( ( (tmpParsers parser) & each . options . each . variables . each
--            %~ (find ( (tokens parser) ^.. each . pattern))
--            & each . tokenType
--            .~ (tokenName parser)
--            & each
--            %~ show
--        )
--          ^. each
--      )
      (parserName parser)
      (toTmpName (name $ head $ tmpParsers parser))         --   (toTmpName (parser ^. tmpParsers ^? _head ^?! _Just ^. name))
      (doAfter parser)

print1 :: CommonParser -> [String]
print1 parser = map (\x -> (toTmpTokenName (fst x)) ++ show (snd x)) (zip [1 :: Int ..] (tokens parser))

help1 :: CommonParser -> [String]
help1 parser = map (\x -> pattern x) (tokens parser)

print2 :: CommonParser -> [TMPParser]
print2 parser = map (\x -> TMPParser { name = name x, tokenType = tokenType x, returnType = returnType x, options = map (\y -> tempFunc y) (options x)}) (tmpParsers parser) where
  tempFunc y = ParserOption{ variables = newvar y, expression = expression y, condition = condition y}
  newvar y = map (\x -> find (help1 parser) x ) (variables y)

find :: [String] -> Context -> Context
find token cotex = case (variable cotex) `elemIndex` token of
  Just n  -> Context {variable = (toTmpTokenName (n + 1)), context = context cotex}    -- cotex & variable .~ (toTmpTokenName (n + 1))
  Nothing -> Context {variable = toTmpName $ variable cotex, context = context cotex}  -- cotex & variable %~ (toTmpName)


-- | Combines all lists via delimeter
join :: [a] -> [[a]] -> [a]
join = intercalate

-- | Combines objects to @String@ via a delimeter
genericJoin :: Show a => String -> [a] -> String
genericJoin delimit l = join delimit (map show l)

-- | Replaces all occurrences that match the regular expression.
replace :: String -> (String -> String) -> String -> String
replace regExp repl str =
  firstMach [] (str =~ regExp :: (String, String, String))
  where
    firstMach :: String -> (String, String, String) -> String
    firstMach acc (x, [], _) = acc ++ x
    firstMach acc (x, y, xs) = firstMach (acc ++ x ++ repl y) (xs =~ regExp)

