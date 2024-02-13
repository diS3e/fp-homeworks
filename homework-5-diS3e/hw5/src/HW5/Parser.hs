module HW5.Parser
  ( parse
  ) where

import Control.Applicative
import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)
import Text.Megaparsec.Char.Lexer (symbol, charLiteral, signed, scientific, space)
import Text.Megaparsec.Char (string, char, hexDigitChar, space1)
import Text.Megaparsec (Parsec, runParser, MonadParsec (try, eof, notFollowedBy), choice, manyTill, satisfy, sepBy1)
import Control.Monad.Combinators.Expr (makeExprParser, Operator (..))

import HW5.Base (HiValue (..), HiExpr (..), HiFun (..), HiAction (..))
import Control.Monad (void)
import Data.Text as T (pack)
import Data.ByteString as B (pack)
import Data.Word
import Data.Char


type Parser = Parsec Void String

parseInBracket :: String -> String -> Parser a -> Parser a
parseInBracket open close parser = ws *> string open *> ws *> parser <* ws <* string close <* ws

parsePriorBracket :: Parser a -> Parser a
parsePriorBracket parser = ws *> (try (parseInBracket "(" ")" (parsePriorBracket parser)) <|> parser) <* ws

parseNumber :: Parser HiValue
parseNumber = HiValueNumber . toRational <$> signed ws scientific

parseCwd :: Parser HiValue
parseCwd = HiValueAction HiActionCwd <$ string "cwd"

parseNow :: Parser HiValue
parseNow = HiValueAction HiActionNow <$ string "now"

ws :: Parser ()
ws = space space1 empty empty

parseBool :: Parser HiValue
parseBool = HiValueBool <$> (pB "true" True <|> pB "false" False)
  where pB str b = do
          void $ string str
          return b

parseNull :: Parser HiValue
parseNull = HiValueNull <$ string "null"

parseString :: Parser HiValue
parseString = HiValueString  . T.pack <$> (char '"' >> manyTill charLiteral (char '"'))

parseList :: Parser HiExpr
parseList = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> parseInBracket "[" "]" (parseSequence exprParser)

parseByteString :: Parser HiValue
parseByteString = HiValueBytes <$> parseInBracket "[#" "#]" (B.pack <$> many (ws *> parseHexDigit <* ws))

parseHexDigit :: Parser Word8
parseHexDigit = do
  a <- hexDigitChar
  b <- hexDigitChar
  return $ read ("0x" ++ [a, b])


parseFunction :: String -> HiFun -> Parser HiValue
parseFunction str constructor = do
  void $ string str
  return $ HiValueFunction constructor

parseArithmeticFunctions :: Parser HiValue
parseArithmeticFunctions = choice [
    parseFunction "add" HiFunAdd
  , parseFunction "sub" HiFunSub
  , parseFunction "mul" HiFunMul
  , parseFunction "div" HiFunDiv
  , parseFunction "and" HiFunAnd
  , parseFunction "or"  HiFunOr
  , parseFunction "less-than" HiFunLessThan
  , parseFunction "greater-than" HiFunGreaterThan
  , parseFunction "equals" HiFunEquals
  , parseFunction "not-less-than" HiFunNotLessThan
  , parseFunction "not-greater-than" HiFunNotGreaterThan
  , parseFunction "not-equals" HiFunNotEquals
  , parseFunction "not" HiFunNot
  , parseFunction "if" HiFunIf
  , parseFunction "length" HiFunLength
  , parseFunction "to-upper" HiFunToUpper
  , parseFunction "to-lower" HiFunToLower
  , parseFunction "reverse" HiFunReverse
  , parseFunction "trim" HiFunTrim
  , parseFunction "list" HiFunList
  , parseFunction "range" HiFunRange
  , parseFunction "fold" HiFunFold
  , parseFunction "pack-bytes" HiFunPackBytes
  , parseFunction "unpack-bytes" HiFunUnpackBytes
  , parseFunction "zip" HiFunZip
  , parseFunction "unzip" HiFunUnzip
  , parseFunction "encode-utf8" HiFunEncodeUtf8
  , parseFunction "decode-utf8" HiFunDecodeUtf8
  , parseFunction "serialise" HiFunSerialise
  , parseFunction "deserialise" HiFunDeserialise
  , parseFunction "read" HiFunRead
  , parseFunction "write" HiFunWrite
  , parseFunction "mkdir" HiFunMkDir
  , parseFunction "cd" HiFunChDir
  , parseFunction "parse-time" HiFunParseTime
  , parseFunction "rand" HiFunRand
  , parseFunction "echo" HiFunEcho
  , parseFunction "count" HiFunCount
  , parseFunction "keys" HiFunKeys
  , parseFunction "values" HiFunValues
  , parseFunction "invert" HiFunInvert
  ]

parseCalled :: Parser HiExpr
parseCalled = ws *> (parsePriorBracket (HiExprValue <$> choice [
    parseArithmeticFunctions
  , parseNumber
  , parseBool
  , parseNull
  , parseString
  , parseByteString
  , parseCwd
  , parseNow ])
 <|> parseList <|> parseDict) <* ws


parseDict :: Parser HiExpr
parseDict = do
  void $ string "{"
  elems <- parseSequence parseDictPair
  void $ string "}"
  return $ HiExprDict elems
  where parseDictPair = do
          key <- exprParser
          void $ string ":"
          value <- exprParser
          return (key, value)


parseLowLevel :: Parser HiExpr
parseLowLevel = ws *> (parseInBracket "(" ")" exprParser <|> parseCalled) <* ws

parseSequence :: Parser a -> Parser [a]
parseSequence parser = (do
      a <- parser
      b <- many $ char ',' *> parser
      return $ a : b) <|> mempty

parseArgs :: Parser [HiExpr]
parseArgs = parseInBracket "(" ")" $ parseSequence exprParser

parseDotAccess :: HiExpr -> Parser HiExpr
parseDotAccess f = (do
  a <-  simpleDotAccess
  parseDotAccess $ HiExprApply f [HiExprValue . HiValueString . T.pack $ a]) <|> parseCalling f
  where simpleDotAccess = do
          void $ string "."
          concat <$> ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'

parseCalling :: HiExpr -> Parser HiExpr
parseCalling f = (do
  args <- try parseArgs
  parseCalling $  HiExprApply f args
  ) <|> return f


parseRun :: HiExpr -> Parser HiExpr
parseRun expr = (do
  void $ string "!" <* ws
  parseRun $ HiExprRun expr
  ) <|> parseCalling expr >>= parseDotAccess

parseHighLevel :: Parser HiExpr
parseHighLevel = do
  v <- parseLowLevel
  do
    ap <- parseCalling v
    dot <- parseDotAccess ap
    parseRun dot



exprParser :: Parser HiExpr
exprParser = makeExprParser (ws *> parseHighLevel <* ws) table

table :: [[Operator Parser HiExpr]]
table = [ [ binary InfixL "*"  HiFunMul
          , InfixL (do
            void $ try (string "/" <* notFollowedBy (satisfy (== '=')))
            return $ \x y -> HiExprApply (HiExprValue $ HiValueFunction HiFunDiv) [x, y])]
        , [ binary InfixL "+"  HiFunAdd
          , binary InfixL "-"  HiFunSub  ]
        , [ binary InfixN "<="  HiFunNotGreaterThan
          , binary InfixN "<" HiFunLessThan
          , binary InfixN ">=" HiFunNotLessThan
          , binary InfixN ">"  HiFunGreaterThan
          , binary InfixN "==" HiFunEquals
          , binary InfixN "/=" HiFunNotEquals ]
        , [ binary InfixR "&&" HiFunAnd ]
        , [ binary InfixR "||" HiFunOr ] ]

binary :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> String -> HiFun -> Operator Parser HiExpr
binary rule name f = rule $ binaryParsing name f

binaryParsing :: String -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
binaryParsing name f = do
  void $ symbol ws name
  return $ \x y -> HiExprApply (HiExprValue $ HiValueFunction f) [x, y]

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (ws *> exprParser <* eof)  ""
