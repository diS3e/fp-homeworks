{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
module HW5.Base
  ( HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiValue (..)
  , toArity
  , Arity (..)
  , HiMonad (..)
  , HiAction (..)
  ) where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.ByteString
import Codec.Serialise
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Data.Map

data HiFun =
    HiFunAdd
  | HiFunSub
  | HiFunMul
  | HiFunDiv

  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
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
-- Special constructors
  | HiFunCallString Text
  | HiFunCallList (Seq HiValue)
  | HiFunCallBytes ByteString
  | HiFunCallDict (Map HiValue HiValue)
  deriving(Show, Eq, Ord, Generic)

instance Serialise HiFun

data Arity =
    Unary HiFun
  | Binary HiFun
  | Ternary HiFun
  | Any HiFun

toArity :: HiFun -> Arity
toArity = \case
  HiFunAdd -> Binary HiFunAdd
  HiFunSub -> Binary HiFunSub
  HiFunMul -> Binary HiFunMul
  HiFunDiv -> Binary HiFunDiv
  HiFunNot -> Unary HiFunNot
  HiFunAnd -> Binary HiFunAnd
  HiFunOr -> Binary HiFunOr
  HiFunLessThan -> Binary HiFunLessThan
  HiFunGreaterThan -> Binary HiFunGreaterThan
  HiFunEquals -> Binary HiFunEquals
  HiFunNotLessThan -> Binary HiFunNotLessThan
  HiFunNotGreaterThan -> Binary HiFunNotGreaterThan
  HiFunNotEquals -> Binary HiFunNotEquals
  HiFunIf -> Ternary HiFunIf
  HiFunLength -> Unary HiFunLength
  HiFunToUpper -> Unary HiFunToUpper
  HiFunToLower -> Unary HiFunToLower
  HiFunReverse -> Unary HiFunReverse
  HiFunTrim -> Unary HiFunTrim
  s@(HiFunCallString _) -> Any s
  s@(HiFunCallList _) -> Any s
  s@(HiFunCallBytes _) -> Any s
  s@(HiFunCallDict _) -> Unary s
  HiFunList -> Any HiFunList
  HiFunRange -> Binary HiFunRange
  HiFunFold -> Binary HiFunFold
  HiFunPackBytes -> Unary HiFunPackBytes
  HiFunUnpackBytes -> Unary HiFunUnpackBytes
  HiFunEncodeUtf8 -> Unary HiFunEncodeUtf8
  HiFunDecodeUtf8 -> Unary HiFunDecodeUtf8
  HiFunZip -> Unary HiFunZip
  HiFunUnzip -> Unary HiFunUnzip
  HiFunSerialise -> Unary HiFunSerialise
  HiFunDeserialise -> Unary HiFunDeserialise
  HiFunRead -> Unary HiFunRead
  HiFunWrite -> Binary HiFunWrite
  HiFunMkDir -> Unary HiFunMkDir
  HiFunChDir -> Unary HiFunChDir
  HiFunParseTime -> Unary HiFunParseTime
  HiFunRand -> Binary HiFunRand
  HiFunEcho -> Unary HiFunEcho

  HiFunCount -> Unary HiFunCount
  HiFunKeys -> Unary HiFunKeys
  HiFunValues -> Unary HiFunValues
  HiFunInvert -> Unary HiFunInvert

data HiValue =
    HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving(Show, Eq, Ord, Generic)

instance Serialise HiValue

data HiExpr =
    HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show)

data HiError =
    HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving(Show, Eq, Ord, Generic)

instance Serialise HiAction

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
