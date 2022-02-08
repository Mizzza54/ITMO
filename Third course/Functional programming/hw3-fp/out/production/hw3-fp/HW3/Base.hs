{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module HW3.Base
  ( HiFun (..)
  , HiValue (..)
  , HiExpr (..)
  , HiError (..)
  , HiAction (..)
  , HiMonad (..)
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Codec.Serialise
import           Data.ByteString (ByteString)
import           Data.Data       (Data)
import           Data.Map        (Map)
import           Data.Sequence   (Seq)
import           Data.Text       (Text)
import           Data.Time       (UTCTime)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

-- | function names (e.g. div, sort, length, ...)
data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNotEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Generic, Data, Typeable)

-- | values (numbers, booleans, strings, ...)
data HiValue
  = HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Generic)

-- | expressions (literals, function calls, ...)
data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq)

-- | evaluation errors (invalid arguments, ...)
data HiError
  = MyUnknownError
  | HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq)

data HiAction
  = HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Generic)

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Show HiFun where
  show HiFunDiv            = "div"
  show HiFunMul            = "mul"
  show HiFunAdd            = "add"
  show HiFunSub            = "sub"
  show HiFunNot            = "not"
  show HiFunAnd            = "and"
  show HiFunOr             = "or"
  show HiFunLessThan       = "less-than"
  show HiFunGreaterThan    = "greater-than"
  show HiFunEquals         = "equals"
  show HiFunNotLessThan    = "not-less-than"
  show HiFunNotGreaterThan = "not-greater-than"
  show HiFunNotEquals      = "not-equals"
  show HiFunIf             = "if"
  show HiFunLength         = "length"
  show HiFunToUpper        = "to-upper"
  show HiFunToLower        = "to-lower"
  show HiFunReverse        = "reverse"
  show HiFunTrim           = "trim"
  show HiFunList           = "list"
  show HiFunRange          = "range"
  show HiFunFold           = "fold"
  show HiFunPackBytes      = "pack-bytes"
  show HiFunUnpackBytes    = "unpack-bytes"
  show HiFunEncodeUtf8     = "encode-utf8"
  show HiFunDecodeUtf8     = "decode-utf8"
  show HiFunZip            = "zip"
  show HiFunUnzip          = "unzip"
  show HiFunSerialise      = "serialise"
  show HiFunDeserialise    = "deserialise"
  show HiFunRead           = "read"
  show HiFunWrite          = "write"
  show HiFunMkDir          = "mkdir"
  show HiFunChDir          = "cd"
  show HiFunParseTime      = "parse-time"
  show HiFunRand           = "rand"
  show HiFunEcho           = "echo"
  show HiFunCount          = "count"
  show HiFunKeys           = "keys"
  show HiFunValues         = "values"
  show HiFunInvert         = "invert"

instance Ord HiValue where
  (HiValueNumber x) <= (HiValueNumber y) = x <= y
  (HiValueNumber _) <= (HiValueBool _)   = False
  (HiValueBool x)   <= (HiValueBool y)   = x <= y
  (HiValueBool _)   <= (HiValueNumber _) = True
  a <= b                                 = show a <= show b

instance Serialise HiValue
instance Serialise HiFun
instance Serialise HiAction

----------------------------------------------------------------------
-- Classes
----------------------------------------------------------------------

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue