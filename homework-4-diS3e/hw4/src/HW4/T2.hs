{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad

import HW4.Types
import HW4.T1 (ExceptState(..))
import Data.Maybe (isNothing, fromJust)
import Data.Char (isSeparator, isDigit)
import GHC.Float (int2Double)
import Data.Foldable (foldl')

newtype ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P parser) s = mapExcept (\(a :# _) -> a) (runES parser (0, s))

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

checkChar :: Char -> Parser Char
checkChar c = mfilter (== c) pChar

parseError :: Parser a
parseError = P $ ES $ Error . ErrorAtPos . fst

pEof :: Parser ()
pEof = P $ ES $ \ps@(pos, s) ->
  case s of
    [] -> Success (() :# ps)
    _ -> Error $ ErrorAtPos pos

instance Alternative Parser where
  empty = parseError
  (P p) <|> (P q) = P $ ES $ \ps ->
    case runES p ps of
      Error _ -> runES q ps
      Success r -> Success r

-- No metohds
instance MonadPlus Parser

pWhitespaces :: Parser String
pWhitespaces = many $ mfilter isSeparator pChar

pNatural :: Parser String
pNatural = some $ mfilter isDigit pChar

-- Можно заменить на int2Double $ digitToInt, но в чате запретили
charToNumber :: Char -> Parser Double
charToNumber c | isDigit c = return $ int2Double $ fromEnum c - fromEnum '0'
               | otherwise = parseError

ent :: String -> Parser Double
ent "" = parseError
ent s = foldl' (\acc x -> do {a <- acc; b <- charToNumber x; return $ a * 10 + b}) (return 0) s

fract :: String -> Parser Double
fract "" = parseError
fract s = foldr (\x acc -> do {a <- acc; b <- charToNumber x; return $ (a + b) / 10}) (return 0) s

pNumber :: Parser Expr
pNumber = do
  { _ <- pWhitespaces
  ; a <- pNatural
  ; entier <- ent a
  ; dot <- optional $ checkChar '.'
  ; if isNothing dot
    then return $ Val entier
    else do 
  { b <- pNatural
  ; fractional <- fract b
  ; _ <- pWhitespaces
  ; return $ Val $ entier + fractional}}

mapCharToOp :: Char -> (Expr -> Expr -> Prim Expr) -> Parser (Expr -> Expr -> Expr)
mapCharToOp c f = do
  { _ <- pWhitespaces
  ; _ <- checkChar c
  ; return (\x y -> Op $ f x y)}

lowPriority :: Parser (Expr -> Expr -> Expr)
lowPriority = mapCharToOp '+' Add <|>
              mapCharToOp '-' Sub

highPriority :: Parser (Expr -> Expr -> Expr)
highPriority = mapCharToOp '*' Mul <|>
               mapCharToOp '/' Div

binaryOperationRule :: Parser (Expr -> Expr -> Expr) -> Parser Expr -> Expr -> Parser Expr
binaryOperationRule pOps pNext x = do
  { op <- optional pOps
  ; if isNothing op
    then return x
    else do 
  { y  <- pNext
  ; binaryOperationRule pOps pNext $ fromJust op x y}}

pLayer :: Parser (Expr -> Expr -> Expr) -> Parser Expr -> Parser Expr
pLayer pOps pNext = do
  { x <- pNext
  ; binaryOperationRule pOps pNext x}

pAddSub :: Parser Expr
pAddSub = pLayer lowPriority pMulDiv

pMulDiv :: Parser Expr
pMulDiv = pLayer highPriority pF

pF :: Parser Expr
pF = pNumber <|> do {
  ; _ <- pWhitespaces >> checkChar '('
  ; y <- pAddSub
  ; _ <- checkChar ')' >> pWhitespaces
  ; return y}

parseExpr :: String -> Except ParseError Expr
parseExpr = runP (do 
  { r <- pAddSub
  ; pEof
  ; return r})
