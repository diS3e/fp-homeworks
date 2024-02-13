{-# LANGUAGE LambdaCase #-}
module HW5.Pretty
  ( prettyValue
  ) where

import Prettyprinter (Doc, pretty, (<+>), slash, lbracket, rbracket, comma, space, encloseSep, lparen, rparen, lbrace, rbrace, colon)
import Prettyprinter.Render.Terminal (AnsiStyle)

import Data.Scientific (fromRationalRepetendUnlimited, toRealFloat)
import HW5.Base (HiValue (..), HiFun (..), HiAction (..))
import GHC.Real (Ratio((:%)))
import Data.Foldable as F (Foldable(toList))
import Numeric (showFloat, showHex)
import Data.ByteString as BS (unpack, ByteString)
import Data.Time (UTCTime)
import Data.Text
import Data.Map as M (Map, toList)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber (n :% 1)) = pretty n
prettyValue (HiValueNumber x) = prettyNumber x
prettyValue (HiValueFunction f) =  pretty $ prettyFun f
prettyValue (HiValueBool b) = pretty $ if b then "true" else "false"
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString text) = prettyText text
prettyValue (HiValueList s) = prettyList $ F.toList s
prettyValue (HiValueBytes b) = prettyBytes b
prettyValue (HiValueAction a) = prettyAction a
prettyValue (HiValueTime t) = prettyTime t
prettyValue (HiValueDict d) = prettyDict d

prettyDict :: Map HiValue HiValue -> Doc AnsiStyle
prettyDict = prettySequence lbrace rbrace (comma <> space)
  (\(key, value) -> prettyValue key <+> colon <+> prettyValue value) . M.toList

prettyString :: String -> Doc AnsiStyle
prettyString s = pretty "\"" <> pretty s <> pretty "\""

prettyText :: Text -> Doc AnsiStyle
prettyText t = pretty "\"" <> pretty t <> pretty "\""

prettyTime :: UTCTime -> Doc AnsiStyle
prettyTime = prettyCall HiFunParseTime . prettyString . show

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction = \case
  HiActionRead p -> prettyCall HiFunRead $ prettyString p
  HiActionWrite p s -> prettyCall HiFunWrite $ prettyString p <> comma <+> prettyBytes s
  HiActionMkDir p -> prettyCall HiFunMkDir $ prettyString p
  HiActionChDir p -> prettyCall HiFunChDir $ prettyString p
  HiActionCwd -> pretty "cwd"
  HiActionNow -> pretty "now"
  HiActionRand a b -> prettyCall HiFunRand $ pretty a <> comma <+> pretty b
  HiActionEcho s -> prettyCall HiFunEcho $ prettyText s

prettyCall :: HiFun -> Doc AnsiStyle -> Doc AnsiStyle
prettyCall f args = pretty (prettyFun f) <> lparen <> args <> rparen

prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes = prettySequence (pretty "[#") (pretty "#]") space prettyHex . BS.unpack
  where prettyHex w = pretty $ showHex (w `div` 16) "" ++ showHex (w `mod` 16) ""

prettySequence :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle -> (a -> Doc AnsiStyle) -> [a] -> Doc AnsiStyle
prettySequence lb rb sep prettyElem = encloseSep (lb <> space) (space <> rb) sep . (prettyElem <$>)

prettyFun :: HiFun -> String
prettyFun = \case
  HiFunAdd -> "add"
  HiFunSub -> "sub"
  HiFunMul -> "mul"
  HiFunDiv -> "div"
  HiFunNot -> "not"
  HiFunAnd -> "and"
  HiFunOr -> "or"
  HiFunLessThan -> "less-than"
  HiFunGreaterThan -> "greater-than"
  HiFunEquals -> "equals"
  HiFunNotLessThan -> "not-less-than"
  HiFunNotGreaterThan -> "not-greater-than"
  HiFunNotEquals -> "not-equals"
  HiFunIf -> "if"
  HiFunLength -> "length"
  HiFunToUpper -> "to-upper"
  HiFunToLower -> "to-lower"
  HiFunReverse -> "reverse"
  HiFunTrim -> "trim"
  HiFunList -> "list"
  HiFunRange -> "range"
  HiFunFold -> "fold"
  HiFunPackBytes -> "pack-bytes"
  HiFunUnpackBytes -> "unpack-bytes"
  HiFunEncodeUtf8 -> "encode-utf8"
  HiFunDecodeUtf8 -> "decode-utf8"
  HiFunZip -> "zip"
  HiFunUnzip -> "unzip"
  HiFunSerialise -> "serialise"
  HiFunDeserialise -> "deserialise"
  HiFunRead -> "read"
  HiFunWrite -> "write"
  HiFunMkDir -> "mkdir"
  HiFunChDir -> "cd"
  HiFunParseTime -> "parse-time"
  HiFunRand -> "rand"
  HiFunEcho -> "echo"
  _ -> "Unexpected function"

prettyList :: [HiValue] -> Doc AnsiStyle
prettyList = prettySequence lbracket rbracket (comma <> space) prettyValue

prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber x@(n:%d) = if removePrimeNumber 2 (removePrimeNumber 5 d) == 1
  then pretty $ showFloat ((toRealFloat $ fst $  fromRationalRepetendUnlimited x) :: Float) ""
  else prettyFractional d $ quotRem n d

removePrimeNumber :: Integer -> Integer -> Integer
removePrimeNumber prime a = if a `mod` prime /= 0
  then a
  else removePrimeNumber prime $ a `div` prime

prettyFractional :: Integer -> (Integer, Integer) -> Doc AnsiStyle
prettyFractional d (0, x) = pretty x <> slash <> pretty d
prettyFractional d (x, y) = pretty x <+> pretty (if y > 0 then "+" else "-") <+> prettyFractional d (0, abs y)

