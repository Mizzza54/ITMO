{-# LANGUAGE OverloadedStrings #-}
module HW3.Pretty
  ( prettyValue
  ) where

import           HW3.Base

import qualified Data.ByteString               as BS
import           Data.Foldable                 (toList)
import qualified Data.Map                      as Map
import           Data.Ratio                    (denominator, numerator)
import           Data.Scientific               (FPFormat (..), formatScientific,
                                                fromRationalRepetendUnlimited)
import qualified Data.Sequence                 as Seq
import           Data.Word                     (Word8)
import           Numeric                       (showHex)
import           Prettyprinter                 (Pretty (..), annotate, enclose,
                                                encloseSep, unsafeViaShow,
                                                viaShow, (<+>))
import           Prettyprinter.Internal.Type   (Doc)
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color)
import           Prettyprinter.Symbols.Ascii   (colon, comma, dquote, lbrace,
                                                lbracket, lparen, rbrace,
                                                rbracket, rparen, space)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue a@(HiValueFunction hiFun) = annotate (color Red) (pretty a)
prettyValue s                         = pretty s

--test = pretty 1 <+> pretty "hello" <+> pretty 1.234

instance Pretty HiValue where
  pretty (HiValueNumber rational)            = pretty $ prettyRational rational
  pretty (HiValueFunction hiFun)             = unsafeViaShow hiFun
  pretty (HiValueBool False)                 = "false"
  pretty (HiValueBool True)                  = "true"
  pretty HiValueNull                         = "null"
  pretty (HiValueString text)                = unsafeViaShow text
  pretty (HiValueList Seq.Empty)             = "[ ]"
  pretty (HiValueList list)                  = encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) (toList (fmap pretty list))
  pretty (HiValueBytes bytes)                = encloseSep (lbracket <> "#" <> space) ("#" <> rbracket) "" (toList (fmap (\x -> (pretty $ prettyHex x) <> space) (BS.unpack bytes)))
  pretty (HiValueAction action)              = pretty action
  pretty (HiValueTime time)                  = "parse-time" <> enclose lparen rparen (enclose dquote dquote (unsafeViaShow time))
  pretty (HiValueDict dict) | Map.null dict  = "{ }"
  pretty (HiValueDict dict)                  = encloseSep (lbrace <> space) (space <> rbrace) (comma <> space) $ map (\pair -> (pretty $ fst pair) <> colon <+> (pretty $ snd pair)) (Map.toList dict)

instance Pretty HiAction where
  pretty (HiActionRead  path)       = "read" <> enclose lparen rparen (unsafeViaShow path)
  pretty (HiActionWrite path bytes) = "write" <> enclose lparen rparen (unsafeViaShow path <> comma <+> pretty (HiValueBytes bytes))
  pretty (HiActionMkDir path)       = "mkdir" <> enclose lparen rparen (unsafeViaShow path)
  pretty (HiActionChDir path)       = "cd" <> enclose lparen rparen (unsafeViaShow path)
  pretty HiActionCwd                = "cwd"
  pretty (HiActionRand l r)         = "rand" <> enclose lparen rparen (pretty l <> comma <+> pretty r)
  pretty (HiActionEcho text)        = "echo" <> enclose lparen rparen (unsafeViaShow text)
  pretty HiActionNow                = "now"

prettyRational :: Rational -> String
prettyRational rational = let quotRem' = quotRem (numerator rational) (denominator rational) in
  case fromRationalRepetendUnlimited rational of
    (scientific, Nothing)  -> case quotRem' of
      (q, 0) -> show q
      _      -> formatScientific Fixed Nothing scientific
    (_, Just _) -> case truncate rational of
      0                -> show (numerator rational) ++ "/" ++ show (denominator rational)
      _ | rational > 0 -> show (fst quotRem') ++ " + " ++ show (snd quotRem') ++ "/" ++ show (denominator rational)
      _                -> show (fst quotRem') ++ " - " ++ show (abs $ snd quotRem') ++ "/" ++ show (denominator rational)

prettyHex :: Word8 -> String
prettyHex w | w < 16 = "0" ++ showHex w ""
            | otherwise = showHex w ""
