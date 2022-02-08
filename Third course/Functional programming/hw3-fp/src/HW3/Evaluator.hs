{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module HW3.Evaluator
  ( eval
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Serialise as Serialise

import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)

import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Time (addUTCTime, diffUTCTime)
import qualified Data.Time as Time
import Data.Word (Word8)

import GHC.Natural (Natural, intToNatural, naturalToInt)

import Text.Read (readMaybe)

import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (..), HiValue (..))

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

data Arity m
  = NotFunction
  | Unary (UnaryFunction m)
  | Binary (BinaryFunction m)
  | Ternary (TernaryFunction m)
  | BinaryTernary (BinaryFunction m) (TernaryFunction m)
  | Variadic (VariadicFunction m)
  | BinaryLazy (BinaryLazyFunction m)
  | TernaryLazy (TernaryLazyFunction m)

type UnaryFunction m = HiValue -> ExceptT HiError m HiValue

type BinaryFunction m = HiValue -> HiValue -> ExceptT HiError m HiValue

type TernaryFunction m = HiValue -> HiValue -> HiValue -> ExceptT HiError m HiValue

type VariadicFunction m = [HiValue] -> ExceptT HiError m HiValue

type BinaryLazyFunction m = HiExpr -> HiExpr -> ExceptT HiError m HiValue

type TernaryLazyFunction m = HiExpr -> HiExpr -> HiExpr -> ExceptT HiError m HiValue

----------------------------------------------------------------------
-- Map HiValue in arity
----------------------------------------------------------------------

wrapHiValue :: HiMonad m => HiValue -> Arity m
wrapHiValue (HiValueFunction HiFunDiv)            = Binary hiFunDiv
wrapHiValue (HiValueFunction HiFunMul)            = Binary hiFunMul
wrapHiValue (HiValueFunction HiFunAdd)            = Binary hiFunAdd
wrapHiValue (HiValueFunction HiFunSub)            = Binary hiFunSub
wrapHiValue (HiValueFunction HiFunNot)            = Unary hiFunNot
wrapHiValue (HiValueFunction HiFunAnd)            = BinaryLazy hiFunAnd
wrapHiValue (HiValueFunction HiFunOr)             = BinaryLazy hiFunOr
wrapHiValue (HiValueFunction HiFunLessThan)       = Binary hiFunLessThan
wrapHiValue (HiValueFunction HiFunGreaterThan)    = Binary hiFunGreaterThan
wrapHiValue (HiValueFunction HiFunEquals)         = Binary hiFunEquals
wrapHiValue (HiValueFunction HiFunNotLessThan)    = Binary hiFunNotLessThan
wrapHiValue (HiValueFunction HiFunNotGreaterThan) = Binary hiFunNotGreaterThan
wrapHiValue (HiValueFunction HiFunNotEquals)      = Binary hiFunNotEquals
wrapHiValue (HiValueFunction HiFunIf)             = TernaryLazy hiFunIf
wrapHiValue (HiValueFunction HiFunLength)         = Unary hiFunLength
wrapHiValue (HiValueFunction HiFunToUpper)        = Unary hiFunToUpper
wrapHiValue (HiValueFunction HiFunToLower)        = Unary hiFunToLower
wrapHiValue (HiValueFunction HiFunReverse)        = Unary hiFunReverse
wrapHiValue (HiValueFunction HiFunTrim)           = Unary hiFunTrim
wrapHiValue (HiValueFunction HiFunList)           = Variadic hiFunList
wrapHiValue (HiValueFunction HiFunRange)          = Binary hiFunRange
wrapHiValue (HiValueFunction HiFunFold)           = Binary hiFunFold
wrapHiValue (HiValueFunction HiFunPackBytes)      = Unary hiFunPackBytes
wrapHiValue (HiValueFunction HiFunUnpackBytes)    = Unary hiFunUnpackBytes
wrapHiValue (HiValueFunction HiFunEncodeUtf8)     = Unary hiFunEncodeUtf8
wrapHiValue (HiValueFunction HiFunDecodeUtf8)     = Unary hiFunDecodeUtf8
wrapHiValue (HiValueFunction HiFunZip)            = Unary hiFunZip
wrapHiValue (HiValueFunction HiFunUnzip)          = Unary hiFunUnzip
wrapHiValue (HiValueFunction HiFunSerialise)      = Unary hiFunSerialise
wrapHiValue (HiValueFunction HiFunDeserialise)    = Unary hiFunDeserialise
wrapHiValue (HiValueFunction HiFunRead)           = Unary hiFunRead
wrapHiValue (HiValueFunction HiFunWrite)          = Binary hiFunWrite
wrapHiValue (HiValueFunction HiFunMkDir)          = Unary hiFunMkDir
wrapHiValue (HiValueFunction HiFunChDir)          = Unary hiFunChDir
wrapHiValue (HiValueFunction HiFunParseTime)      = Unary hiFunParseTime
wrapHiValue (HiValueFunction HiFunRand)           = Binary hiFunRand
wrapHiValue (HiValueFunction HiFunEcho)           = Unary hiFunEcho
wrapHiValue (HiValueFunction HiFunCount)          = Unary hiFunCount
wrapHiValue (HiValueFunction HiFunKeys)           = Unary hiFunKeys
wrapHiValue (HiValueFunction HiFunValues)         = Unary hiFunValues
wrapHiValue (HiValueFunction HiFunInvert)         = Unary hiFunInvert
wrapHiValue (HiValueNumber _)                     = NotFunction
wrapHiValue HiValueNull                           = NotFunction
wrapHiValue (HiValueBool _)                       = NotFunction
wrapHiValue (HiValueString _)                     = BinaryTernary hiFunIndex hiFunSlice
wrapHiValue (HiValueList _)                       = BinaryTernary hiFunIndex hiFunSlice
wrapHiValue (HiValueBytes _)                      = BinaryTernary hiFunIndex hiFunSlice
wrapHiValue (HiValueAction _)                     = NotFunction
wrapHiValue (HiValueDict _)                       = Binary hiFunIndex
wrapHiValue (HiValueTime _)                       = NotFunction

----------------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------------

eHiExpr :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eHiExpr (HiExprValue hiValue)   = return hiValue
eHiExpr (HiExprApply func args) = eHiExprApply func args
eHiExpr (HiExprRun hiExpr)      = eHiExprRun hiExpr
eHiExpr (HiExprDict pairs)      = eHiExprDict pairs

eHiExprApply :: HiMonad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
eHiExprApply (HiExprValue hiValue) args = let arity = wrapHiValue hiValue in case arity of
  NotFunction       -> throwError HiErrorInvalidFunction
  BinaryTernary _ _ -> eApplyFunc arity (HiExprValue hiValue : args)
  _                 -> eApplyFunc arity args
eHiExprApply (HiExprApply func' args') args = do
  inner <- eHiExprApply func' args'
  case inner of
    HiValueDict _ -> eHiExprApply (HiExprValue inner) (HiExprValue inner : args)
    _             -> eHiExprApply (HiExprValue inner) args
eHiExprApply (HiExprRun action) args = do
  inner <- eHiExprRun action
  eHiExprApply (HiExprValue inner) args
eHiExprApply (HiExprDict dict) args = do
  dict' <- eHiExprDict dict
  eHiExprApply (HiExprValue dict') (HiExprValue dict' : args)

eHiExprRun :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eHiExprRun (HiExprValue (HiValueAction action)) = lift $ runAction action
eHiExprRun (HiExprApply func args) = do
  inner <- eHiExprApply func args
  eHiExprRun (HiExprValue inner)
eHiExprRun _ = throwError HiErrorInvalidArgument

eHiExprDict :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m HiValue
eHiExprDict pairs = do
  res <- liftPair (map (bimap eHiExpr eHiExpr) pairs)
  return $ HiValueDict $ Map.fromList res

----------------------------------------------------------------------
-- Apply a function of different arity
----------------------------------------------------------------------

eApplyFunc :: HiMonad m => Arity m -> [HiExpr] -> ExceptT HiError m HiValue
eApplyFunc (Unary func) [arg]                       = do
  eArg <- eHiExpr arg
  func eArg
eApplyFunc (Binary func) [l, r]                     = do
  eL <- eHiExpr l
  eR <- eHiExpr r
  eL `func` eR
eApplyFunc (Ternary func) [arg1, arg2, arg3]        = do
  eArg1 <- eHiExpr arg1
  eArg2 <- eHiExpr arg2
  eArg3 <- eHiExpr arg3
  func eArg1 eArg2 eArg3
eApplyFunc (BinaryTernary binary _) args@[_, _]     = eApplyFunc (Binary binary) args
eApplyFunc (BinaryTernary _ ternary) args@[_, _, _] = eApplyFunc (Ternary ternary) args
eApplyFunc (BinaryLazy func) [l, r] = do
  l `func` r
eApplyFunc (TernaryLazy func) [arg1, arg2, arg3]    = do
  func arg1 arg2 arg3
eApplyFunc (Variadic func) args = do
  list <- traverse eHiExpr args
  func list
eApplyFunc _ _                                       = throwError HiErrorArityMismatch

----------------------------------------------------------------------
-- Implementation unary functions
----------------------------------------------------------------------

hiFunNot :: HiMonad m => UnaryFunction m
hiFunNot (HiValueBool bool) = return . HiValueBool $ not bool
hiFunNot _                  = throwError HiErrorInvalidArgument

hiFunLength :: HiMonad m => UnaryFunction m
hiFunLength hiValue
  | lengthHiValue hiValue == -1 = throwError HiErrorInvalidArgument
  | otherwise                   = return . HiValueNumber . toRational . lengthHiValue $ hiValue

hiFunToUpper :: HiMonad m => UnaryFunction m
hiFunToUpper (HiValueString text) = return . HiValueString . Text.toUpper $ text
hiFunToUpper _                    = throwError HiErrorInvalidArgument

hiFunToLower :: HiMonad m => UnaryFunction m
hiFunToLower (HiValueString text) = return . HiValueString . Text.toLower $ text
hiFunToLower _                    = throwError HiErrorInvalidArgument

hiFunReverse :: HiMonad m => UnaryFunction m
hiFunReverse (HiValueString text) = return . HiValueString . Text.reverse $ text
hiFunReverse (HiValueList list)   = return . HiValueList . Seq.reverse $ list
hiFunReverse (HiValueBytes bytes) = return . HiValueBytes . BS.reverse $ bytes
hiFunReverse _                    = throwError HiErrorInvalidArgument

hiFunTrim :: HiMonad m => UnaryFunction m
hiFunTrim (HiValueString text) = return . HiValueString . Text.strip $ text
hiFunTrim _                    = throwError HiErrorInvalidArgument

hiFunPackBytes :: HiMonad m => UnaryFunction m
hiFunPackBytes (HiValueList list) = do
  word8List <- traverse hiValueToWord8 list
  return . HiValueBytes . BS.pack . toList $ word8List
hiFunPackBytes _                  = throwError HiErrorInvalidArgument

hiFunUnpackBytes :: HiMonad m => UnaryFunction m
hiFunUnpackBytes (HiValueBytes bytes) = do
  let word8List = BS.unpack bytes
  let list = fmap word8ToHiValue word8List
  return . HiValueList . Seq.fromList $ list
hiFunUnpackBytes _                    = throwError HiErrorInvalidArgument

hiFunEncodeUtf8 :: HiMonad m => UnaryFunction m
hiFunEncodeUtf8 (HiValueString text) = return . HiValueBytes . Encoding.encodeUtf8 $ text
hiFunEncodeUtf8 _                    = throwError HiErrorInvalidArgument

hiFunDecodeUtf8 :: HiMonad m => UnaryFunction m
hiFunDecodeUtf8 (HiValueBytes bytes) = case Encoding.decodeUtf8' bytes of
  Left _     -> return HiValueNull
  Right text -> return . HiValueString $ text
hiFunDecodeUtf8 _                    = throwError HiErrorInvalidArgument

hiFunZip :: HiMonad m => UnaryFunction m
hiFunZip (HiValueBytes bytes) = return . handleLazyByteString bestCompress $ bytes
hiFunZip _                    = throwError HiErrorInvalidArgument

hiFunUnzip :: HiMonad m => UnaryFunction m
hiFunUnzip (HiValueBytes bytes) = return . handleLazyByteString Zlib.decompress $ bytes
hiFunUnzip _                    = throwError HiErrorInvalidArgument

hiFunSerialise :: HiMonad m => UnaryFunction m
hiFunSerialise = return . HiValueBytes . BSL.toStrict . Serialise.serialise

hiFunDeserialise :: HiMonad m => UnaryFunction m
hiFunDeserialise (HiValueBytes bytes) = case Serialise.deserialiseOrFail (BSL.fromStrict bytes) of
  Left _  -> return HiValueNull
  Right r -> return r
hiFunDeserialise _                    = throwError HiErrorInvalidArgument

hiFunRead :: HiMonad m => UnaryFunction m
hiFunRead (HiValueString text) = return . HiValueAction . HiActionRead . Text.unpack $ text
hiFunRead _                    = throwError HiErrorInvalidArgument

hiFunMkDir :: HiMonad m => UnaryFunction m
hiFunMkDir (HiValueString text) = return . HiValueAction . HiActionMkDir . Text.unpack $ text
hiFunMkDir _                    = throwError HiErrorInvalidArgument

hiFunChDir :: HiMonad m => UnaryFunction m
hiFunChDir (HiValueString text) = return . HiValueAction . HiActionChDir . Text.unpack $ text
hiFunChDir _                    = throwError HiErrorInvalidArgument

hiFunParseTime :: HiMonad m => UnaryFunction m
hiFunParseTime (HiValueString text) = do
  let maybeTime = readMaybe $ Text.unpack text :: Maybe Time.UTCTime
  case maybeTime of
    Just time -> return . HiValueTime $ time
    Nothing   -> return HiValueNull
hiFunParseTime _                    = throwError HiErrorInvalidArgument

hiFunEcho :: HiMonad m => UnaryFunction m
hiFunEcho (HiValueString text) = return . HiValueAction . HiActionEcho $ text
hiFunEcho _                    = throwError HiErrorInvalidArgument

hiFunCount :: HiMonad m => UnaryFunction m
hiFunCount (HiValueString text) = countsHiValue text Text.unpack (HiValueString . Text.singleton)
hiFunCount (HiValueList list)   = countsHiValue list toList id
hiFunCount (HiValueBytes bytes) = countsHiValue bytes BS.unpack (HiValueNumber . toRational)
hiFunCount _                    = throwError HiErrorInvalidArgument

hiFunKeys :: HiMonad m => UnaryFunction m
hiFunKeys (HiValueDict dict) = return . HiValueList . Seq.fromList . Map.keys $ dict
hiFunKeys _                  = throwError HiErrorInvalidArgument

hiFunValues :: HiMonad m => UnaryFunction m
hiFunValues (HiValueDict dict) = return . HiValueList . Seq.fromList . Map.elems $ dict
hiFunValues _                  = throwError HiErrorInvalidArgument

hiFunInvert :: HiMonad m => UnaryFunction m
hiFunInvert (HiValueDict dict) = return . HiValueDict . invertMap $ dict
hiFunInvert _                  = throwError HiErrorInvalidArgument

----------------------------------------------------------------------
-- Implementation binary functions
----------------------------------------------------------------------

hiFunDiv :: HiMonad m => BinaryFunction m
hiFunDiv (HiValueNumber ln) (HiValueNumber rn)       = case rn of
  0 -> throwError HiErrorDivideByZero
  _ -> return $ HiValueNumber (ln / rn)
hiFunDiv (HiValueString lText) (HiValueString rText) = return . HiValueString $ lText <> "/" <> rText
hiFunDiv _ _                                         = throwError HiErrorInvalidArgument

hiFunMul :: HiMonad m => BinaryFunction m
hiFunMul (HiValueNumber ln) (HiValueNumber rn) = return $ HiValueNumber (ln * rn)
hiFunMul hiValue (HiValueNumber rational)      = case (rationalIsInteger rational, hiValue) of
  (Just n, HiValueString text) | n > 0 -> stimesHiValue n HiValueString text
  (Just n, HiValueList list)   | n > 0 -> stimesHiValue n HiValueList list
  (Just n, HiValueBytes bytes) | n > 0 -> stimesHiValue n HiValueBytes bytes
  (_, _)                               -> throwError HiErrorInvalidArgument
hiFunMul _ _                                   = throwError HiErrorInvalidArgument

hiFunAdd :: HiMonad m => BinaryFunction m
hiFunAdd (HiValueNumber ln) (HiValueNumber rn)       = return $ HiValueNumber (ln + rn)
hiFunAdd (HiValueString lText) (HiValueString rText) = return . HiValueString $ lText <> rText
hiFunAdd (HiValueList lList) (HiValueList rList)     = return . HiValueList $ lList <> rList
hiFunAdd (HiValueBytes lBytes) (HiValueBytes rBytes) = return . HiValueBytes $ lBytes <> rBytes
hiFunAdd (HiValueTime time) (HiValueNumber rational) = return . HiValueTime $ addUTCTime (fromRational rational) time
hiFunAdd _ _                                         = throwError HiErrorInvalidArgument

hiFunSub :: HiMonad m => BinaryFunction m
hiFunSub (HiValueNumber ln) (HiValueNumber rn)   = return $ HiValueNumber (ln - rn)
hiFunSub (HiValueTime lTime) (HiValueTime rTime) = return . HiValueNumber . toRational $ diffUTCTime lTime rTime
hiFunSub _ _                                     = throwError HiErrorInvalidArgument

hiFunAnd :: HiMonad m => BinaryLazyFunction m
hiFunAnd (HiExprValue (HiValueBool False)) _ = return . HiValueBool $ False
hiFunAnd (HiExprValue HiValueNull) _         = return HiValueNull
hiFunAnd lHiValue rHiValue                   = do
  l <- eHiExpr lHiValue
  case l of
    HiValueBool False -> return . HiValueBool $ False
    HiValueBool True  -> eHiExpr rHiValue
    HiValueNull       -> return HiValueNull
    _                 -> eHiExpr rHiValue
    
    

hiFunOr :: HiMonad m => BinaryLazyFunction m
hiFunOr lHiExpr rHiExpr = do
  l <- eHiExpr lHiExpr
  case l of
    (HiValueBool False) -> eHiExpr rHiExpr
    HiValueNull         -> eHiExpr rHiExpr
    (HiValueBool True)  -> return . HiValueBool $ True
    r                   -> return r


hiFunLessThan :: HiMonad m => BinaryFunction m
hiFunLessThan l r = return . HiValueBool $ l < r

hiFunGreaterThan :: HiMonad m => BinaryFunction m
hiFunGreaterThan l r = return . HiValueBool $ l > r

hiFunEquals :: HiMonad m => BinaryFunction m
hiFunEquals l r = return . HiValueBool $ l == r

hiFunNotLessThan :: HiMonad m => BinaryFunction m
hiFunNotLessThan l r = return . HiValueBool $ l >= r

hiFunNotGreaterThan :: HiMonad m => BinaryFunction m
hiFunNotGreaterThan l r = return . HiValueBool $ l <= r

hiFunNotEquals :: HiMonad m => BinaryFunction m
hiFunNotEquals l r = return . HiValueBool $ l /= r

hiFunIndex :: HiMonad m => BinaryFunction m
hiFunIndex (HiValueDict dict) key           = case (Map.!?) dict key of
  Just v  -> return v
  Nothing -> return HiValueNull
hiFunIndex hiValue (HiValueNumber rational) = case rationalIsInteger rational of
  Just num | num < lengthHiValue hiValue && num >= 0 -> case hiValue of
    (HiValueString text) -> return . HiValueString . Text.pack $ [Text.index text (fromInteger num)]
    (HiValueList list)   -> return $ Seq.index list (fromInteger num)
    (HiValueBytes bytes) -> return . HiValueNumber . toRational $ BS.index bytes (fromInteger num)
    _                    -> throwError HiErrorInvalidArgument
  _                                                   -> return HiValueNull
hiFunIndex _ _                              = throwError HiErrorInvalidArgument

hiFunRange :: HiMonad m => BinaryFunction m
hiFunRange (HiValueNumber ln) (HiValueNumber rn) = return . HiValueList . Seq.fromList $ map HiValueNumber [ln..rn]
hiFunRange _ _                                   = throwError HiErrorInvalidArgument

hiFunFold :: HiMonad m => BinaryFunction m
hiFunFold _ (HiValueList Seq.Empty)             = return HiValueNull
hiFunFold funcName (HiValueList (x Seq.:<| xs)) = let arity = wrapHiValue funcName in case arity of
  Binary _     -> fold arity x xs
  BinaryLazy _ -> fold arity x xs
  _            -> throwError HiErrorInvalidArgument
hiFunFold _ _                                   = throwError HiErrorInvalidArgument

hiFunWrite :: HiMonad m => BinaryFunction m
hiFunWrite (HiValueString path) (HiValueString text) = return . HiValueAction $ HiActionWrite (Text.unpack path) (Encoding.encodeUtf8 text)
hiFunWrite (HiValueString path) (HiValueBytes bytes) = return . HiValueAction $ HiActionWrite (Text.unpack path) bytes
hiFunWrite _ _                                       = throwError HiErrorInvalidArgument

hiFunRand :: HiMonad m => BinaryFunction m
hiFunRand (HiValueNumber l) (HiValueNumber r) = case (rationalIsInt l, rationalIsInt r) of
  (Just ln, Just rn) -> return . HiValueAction $ HiActionRand ln rn
  _                  -> throwError HiErrorInvalidArgument
hiFunRand _ _                                 = throwError HiErrorInvalidArgument

----------------------------------------------------------------------
-- Implementation triple functions
----------------------------------------------------------------------

hiFunIf :: HiMonad m => TernaryLazyFunction m
hiFunIf (HiExprValue (HiValueBool bool)) exprTrue exprFalse = eHiExpr $ if bool then exprTrue else exprFalse
hiFunIf condition exprTrue exprFalse                        = do
  eCondition <- eHiExpr condition
  case eCondition of
    HiValueBool bool -> eHiExpr $ if bool then exprTrue else exprFalse
    _                -> throwError HiErrorInvalidArgument

hiFunSlice :: HiMonad m => TernaryFunction m
hiFunSlice hiValue (HiValueNumber ln) (HiValueNumber rn) = case (rationalIsInteger ln, rationalIsInteger rn) of
  (Just ln', Just rn') -> sliceHiValue (fromInteger ln') (fromInteger rn') hiValue
  (_, _)               -> throwError HiErrorInvalidArgument
hiFunSlice hiValue HiValueNull (HiValueNumber rn)        = case rationalIsInteger rn of
  Just rn' -> sliceHiValue 0 (fromInteger rn') hiValue
  _        -> throwError HiErrorInvalidArgument
hiFunSlice hiValue (HiValueNumber ln) HiValueNull        = case rationalIsInteger ln of
  Just ln' -> sliceHiValue (fromInteger ln') (fromInteger . lengthHiValue $ hiValue) hiValue
  _        -> throwError HiErrorInvalidArgument
hiFunSlice hiValue HiValueNull HiValueNull               = return hiValue
hiFunSlice _ _ _                                         = throwError HiErrorInvalidArgument

----------------------------------------------------------------------
-- Implementation Variadic functions
----------------------------------------------------------------------

hiFunList :: HiMonad m => VariadicFunction m
hiFunList args = return . HiValueList $ Seq.fromList args

----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

lengthHiValue :: HiValue -> Integer
lengthHiValue (HiValueString text) = toInteger . Text.length $ text
lengthHiValue (HiValueList list)   = toInteger . Seq.length  $ list
lengthHiValue (HiValueBytes bytes) = toInteger . BS.length   $ bytes
lengthHiValue _                    = -1

sliceHiValue :: HiMonad m => Int -> Int -> HiValue -> ExceptT HiError m HiValue
sliceHiValue l r hiValue =
  case hiValue of
    (HiValueString text) -> return . HiValueString . Text.pack $ slice l r (Text.unpack text)
    (HiValueList list)   -> return . HiValueList . Seq.fromList $ slice l r (toList list)
    (HiValueBytes bytes) -> return . HiValueBytes . BS.pack $ slice l r (BS.unpack bytes)
    _                    -> throwError HiErrorInvalidArgument

rationalIsInteger :: Rational -> Maybe Integer
rationalIsInteger rational = case quotRem (numerator rational) (denominator rational) of
  (num, 0) -> Just $ fromIntegral num
  _        -> Nothing

integerIsInt :: Integer -> Maybe Int
integerIsInt integer = let int = fromIntegral integer in 
  if integer == fromIntegral int 
  then 
    Just int 
  else 
    Nothing
    
rationalIsInt :: Rational -> Maybe Int
rationalIsInt x = integerIsInt =<< (rationalIsInteger x)

slice :: Int -> Int -> [a] -> [a]
slice start end list
  | start >= 0 && end >= 0 = take (end - start) . drop start $ list
  | end < 0 = let end'     = length list + end in slice start end' list
  | start < 0 = let start' = length list + start in slice start' end list
slice _ _ _ = []

fold :: HiMonad m => Arity m -> HiValue -> Seq HiValue -> ExceptT HiError m HiValue
fold (Binary func) acc (x :<| xs) = do
  res <- acc `func` x
  fold (Binary func) res xs
fold (BinaryLazy func) acc (x Seq.:<| xs) = do
  res <- HiExprValue acc `func` HiExprValue x
  fold (BinaryLazy func) res xs
fold _ acc Seq.Empty = return acc
fold _ _ _           = throwError HiErrorInvalidArgument

hiValueToWord8 :: HiMonad m => HiValue -> ExceptT HiError m Word8
hiValueToWord8 (HiValueNumber rational) = case rationalIsInteger rational of
  Just num -> return $ fromInteger . toInteger $ num
  Nothing  -> throwError HiErrorInvalidArgument
hiValueToWord8 _ = throwError HiErrorInvalidArgument

word8ToHiValue :: Word8 -> HiValue
word8ToHiValue = HiValueNumber . fromInteger . toInteger

countsHiValue :: (Monad m, Eq b) => a -> (a -> [b]) -> (b -> HiValue) -> m HiValue
countsHiValue value unpack handle = return . HiValueDict . Map.fromList $ map (bimap handle (HiValueNumber . toRational)) $ counts (unpack value)

counts :: Eq a => [a] -> [(a,Int)]
counts = map headLength . classify

headLength :: [a] -> (a,Int)
headLength x = (head x, length x)

classify :: Eq a => [a] -> [[a]]
classify []     = []
classify (x:xs) = (x : filter (== x) xs) : classify (filter (/= x) xs)

liftPair :: Monad m => [(m a, m b)] -> m [(a, b)]
liftPair = mapM func
  where
  func (a, b) = do
    a1 <- a
    b1 <- b
    return (a1, b1)

bestCompress :: BSL.ByteString -> BSL.ByteString
bestCompress = Zlib.compressWith Zlib.defaultCompressParams { Zlib.compressLevel = Zlib.bestCompression }

handleLazyByteString :: (BSL.ByteString -> BSL.ByteString) -> BS.ByteString -> HiValue
handleLazyByteString func = HiValueBytes . BSL.toStrict . func . BSL.fromStrict

invertMap :: Map HiValue HiValue -> Map HiValue HiValue
invertMap m = HiValueList <$> Map.fromListWith (Seq.><) pairs
    where pairs = [(v, Seq.fromList [k]) | (k, v) <- Map.toList m]

stimesHiValue :: (HiMonad m, Monoid a) => Integer -> (a -> HiValue) -> a -> ExceptT HiError m HiValue
stimesHiValue 0 constructor _     = return . constructor $ mempty
stimesHiValue n constructor value = return . constructor $ stimes n value

----------------------------------------------------------------------
-- Evaluation runner
----------------------------------------------------------------------

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . eHiExpr
