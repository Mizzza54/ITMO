{-# LANGUAGE OverloadedStrings #-}
module HW3.Pretty
  ( prettyValue
  , prettyError
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------  

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
import           Prettyprinter                 (Doc, Pretty (..),
                                                enclose, encloseSep,
                                                unsafeViaShow, viaShow, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)
import           Prettyprinter.Symbols.Ascii   (colon, comma, dquote, lbrace,
                                                lbracket, lparen, rbrace,
                                                rbracket, rparen, space)

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Pretty HiValue where
  pretty (HiValueNumber rational)            = prettyRational rational
  pretty (HiValueFunction hiFun)             = unsafeViaShow hiFun
  pretty (HiValueBool False)                 = "false"
  pretty (HiValueBool True)                  = "true"
  pretty HiValueNull                         = "null"
  pretty (HiValueString text)                = viaShow text
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

----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

prettyRational :: Rational -> Doc ann
prettyRational rational = let (qout', rem') = quotRem (numerator rational) (denominator rational) in
  case fromRationalRepetendUnlimited rational of
    (scientific, Nothing)  -> case (qout', rem') of
      (q, 0) -> pretty q
      _      -> pretty $ formatScientific Fixed Nothing scientific
    (_, Just _) -> case qout' of
      0                -> unsafeViaShow (numerator rational) <> "/" <> unsafeViaShow (denominator rational)
      _ | rational > 0 -> unsafeViaShow qout' <> " + " <> unsafeViaShow rem' <> "/" <> unsafeViaShow (denominator rational)
      _                -> unsafeViaShow qout' <> " - " <> unsafeViaShow (abs $ rem') <> "/" <> unsafeViaShow (denominator rational)

prettyHex :: Word8 -> String
prettyHex w | w < 16 = "0" ++ showHex w ""
            | otherwise = showHex w ""

----------------------------------------------------------------------
-- Export pretty
----------------------------------------------------------------------

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = pretty

prettyError :: HiError -> Doc AnsiStyle
prettyError e = unsafeViaShow e