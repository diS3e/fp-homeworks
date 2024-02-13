{-# LANGUAGE LambdaCase #-}
module HW5.Evaluator
  ( eval
  ) where

import HW5.Base
import Control.Monad.Trans.Except (ExceptT (..), throwE, runExceptT)
import Data.Text as T ( reverse, singleton, drop,
      take,
      length,
      index,
      strip,
      toLower,
      toUpper, unpack)
import Data.Semigroup (Semigroup(..))
import GHC.Real (Ratio((:%)))
import Data.Sequence as S (fromList, Seq ((:<|), Empty), length, index, drop, take, reverse)
import Data.Foldable (toList)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Codec.Compression.Zlib (bestCompression, compressWith, CompressParams (compressLevel), defaultCompressParams, decompressWith, defaultDecompressParams)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Codec.Serialise (serialise, deserialiseOrFail)
import Data.ByteString as BS (length, index, drop, take, reverse, pack, unpack)
import Text.Read (readMaybe)
import Data.Time.Clock
import Data.Tuple (swap)
import Data.Function
import Control.Monad
import Data.Map as M (empty, insert, lookup, insertWith, map, keys, elems, assocs, mapKeys, fromListWith)
import Control.Applicative



type Evaluation m = ExceptT HiError m HiValue

unFunction :: HiMonad m => (HiExpr -> Evaluation m) -> [HiExpr] -> ExceptT HiError m HiValue
unFunction op [a] = op a
unFunction _ _ = throwE HiErrorArityMismatch

biFunction :: HiMonad m => (HiExpr -> HiExpr -> Evaluation m) -> [HiExpr] -> ExceptT HiError m HiValue
biFunction op [a, b] = a `op` b
biFunction _ _ = throwE HiErrorArityMismatch

teFunction :: HiMonad m => (HiExpr -> HiExpr -> HiExpr -> Evaluation m) -> [HiExpr] -> ExceptT HiError m HiValue
teFunction op [a, b, c] = op a b c
teFunction _ _ = throwE HiErrorArityMismatch

toOperation :: HiMonad m => Arity -> [HiExpr] -> ExceptT HiError m HiValue
toOperation (Binary f) = biFunction (biFunctionSet f)
toOperation (Unary f) = unFunction (unFunctionSet f)
toOperation (Ternary f) = teFunction (teFunctionSet f)
toOperation (Any f) = anyFunction f

getByIndex :: HiMonad m => Int -> (Int -> HiValue) -> HiExpr -> Evaluation m
getByIndex len f a = do
  x <- evalT a
  case x of
    (HiValueNumber (ind :% 1)) ->
      if 0 <= ind && ind < toInteger len
        then return $ f (fromInteger ind)
        else return HiValueNull
    _                   -> throwE HiErrorInvalidArgument

slice :: HiMonad m => Int -> (Int -> Int -> HiValue) -> HiExpr -> HiExpr -> Evaluation m
slice sz corange a b = do
  left <- evalT a
  right <- evalT b
  range left right
  where
    len = toInteger sz
    range left right =
          case (left, right) of
            (HiValueNull, r) -> range (HiValueNumber 0) r
            (l, HiValueNull) -> range l (HiValueNumber (toRational len))
            (HiValueNumber (l :% 1), HiValueNumber (r :% 1)) -> return $ corange
              (fromInteger (if l < 0 then max (l + len) 0 else l))
              (fromInteger (if r < 0 then max (r + len) 0 else r))
            _ -> throwE HiErrorInvalidArgument

anyFunction :: HiMonad m => HiFun -> [HiExpr] -> Evaluation m
anyFunction (HiFunCallString text) [a] = getByIndex (T.length text) (HiValueString . T.singleton . T.index text) a
anyFunction (HiFunCallString t) [a, b] = slice (T.length t)
  (\ll rr -> HiValueString . T.drop ll . T.take rr $ t) a b

anyFunction (HiFunCallList l) [a] = getByIndex (S.length l) (S.index l) a
anyFunction (HiFunCallList l) [a, b] = slice (S.length l)
  (\ll rr -> HiValueList . S.drop ll . S.take rr $ l) a b

anyFunction (HiFunCallBytes b) [a] = getByIndex (BS.length b) (HiValueNumber . toRational . BS.index  b) a
anyFunction (HiFunCallBytes bytes) [a, b] = slice (BS.length bytes)
  (\ll rr -> HiValueBytes . BS.drop ll . BS.take rr $ bytes) a b
anyFunction HiFunList arr = do
  elms <- mapM evalT arr
  return $ HiValueList $ fromList elms



anyFunction _ _ = throwE HiErrorInvalidArgument

unFunctionSet :: HiMonad m => HiFun -> HiExpr -> Evaluation m
unFunctionSet f a = do
  x <- evalT a
  unActiveFunctions f x

unActiveFunctions :: HiMonad m => HiFun -> HiValue -> Evaluation m
unActiveFunctions HiFunNot (HiValueBool a) = return $ HiValueBool $ not a

unActiveFunctions HiFunLength (HiValueString a) = return $ HiValueNumber $ toRational $ T.length a
unActiveFunctions HiFunLength (HiValueBytes a) = return $ HiValueNumber $ toRational $ BS.length a
unActiveFunctions HiFunLength (HiValueList a) = return $ HiValueNumber $ toRational $ S.length a

unActiveFunctions HiFunReverse (HiValueString a) = return $ HiValueString $ T.reverse a
unActiveFunctions HiFunReverse (HiValueBytes a) = return $ HiValueBytes $ BS.reverse a
unActiveFunctions HiFunReverse (HiValueList a) = return $ HiValueList $ S.reverse a

unActiveFunctions HiFunToUpper (HiValueString a) = return $ HiValueString $ T.toUpper a
unActiveFunctions HiFunToLower (HiValueString a) = return $ HiValueString $ T.toLower a
unActiveFunctions HiFunTrim (HiValueString a) = return $ HiValueString $ T.strip a
unActiveFunctions HiFunPackBytes (HiValueList a) = HiValueBytes . pack . toList <$> mapM
  (\case
  (HiValueNumber (n :% 1)) -> return $ toEnum $ fromInteger n
  _ -> throwE HiErrorInvalidArgument) a
unActiveFunctions HiFunUnpackBytes (HiValueBytes b) = return $ HiValueList $ fromList $ HiValueNumber . toRational . fromEnum <$> BS.unpack b
unActiveFunctions HiFunEncodeUtf8 (HiValueString str) = return $ HiValueBytes $ encodeUtf8 str
unActiveFunctions HiFunDecodeUtf8 (HiValueBytes b) =  case decodeUtf8' b of
  Left _ -> return HiValueNull
  Right r -> return $ HiValueString r
unActiveFunctions HiFunZip (HiValueBytes b) = return $ HiValueBytes $ toStrict $ compressWith defaultCompressParams{compressLevel = bestCompression} $ fromStrict b
unActiveFunctions HiFunUnzip (HiValueBytes b) = return $ HiValueBytes $ toStrict $ decompressWith defaultDecompressParams $ fromStrict b
unActiveFunctions HiFunSerialise v = return . HiValueBytes . toStrict $  serialise v
unActiveFunctions HiFunDeserialise (HiValueBytes b) = do
  case deserialiseOrFail $ fromStrict b of
    Left _ -> throwE HiErrorInvalidArgument
    Right v -> return v
unActiveFunctions HiFunRead (HiValueString s) = return $ HiValueAction $ HiActionRead $ T.unpack s
unActiveFunctions HiFunMkDir (HiValueString s) = return $ HiValueAction $ HiActionMkDir $ T.unpack s
unActiveFunctions HiFunChDir (HiValueString s) = return $ HiValueAction $ HiActionChDir $ T.unpack s
unActiveFunctions HiFunParseTime (HiValueString s) =
  case readMaybe $ T.unpack s :: Maybe UTCTime of
    Nothing -> return HiValueNull
    Just p  -> return $ HiValueTime p
unActiveFunctions HiFunEcho (HiValueString s) = return $ HiValueAction $ HiActionEcho s
unActiveFunctions (HiFunCallDict m) key = case M.lookup key m of
  Nothing -> return HiValueNull
  Just val -> return val
unActiveFunctions HiFunCount (HiValueString s) = count (T.unpack s) (HiValueString . T.singleton)
unActiveFunctions HiFunCount (HiValueList s) = count (toList s) id
unActiveFunctions HiFunCount (HiValueBytes s) = count (BS.unpack s) (HiValueNumber . toRational)
unActiveFunctions HiFunKeys (HiValueDict d) = return $ HiValueList $ S.fromList $ M.keys d
unActiveFunctions HiFunValues (HiValueDict d) = return $ HiValueList $ S.fromList $ M.elems d
unActiveFunctions HiFunInvert (HiValueDict d) = return $ HiValueDict $ M.map (HiValueList . S.fromList) (M.fromListWith (++) (swap <$> M.assocs (M.mapKeys (:[]) d)))
unActiveFunctions _ _ = throwE HiErrorInvalidArgument

count :: HiMonad m => [a] -> (a -> HiValue) -> Evaluation m
count a f = return $ HiValueDict $ M.map HiValueNumber (foldl 
  (\m k -> insertWith (+) (f k) 1 m) M.empty a)

biFunctionSet :: HiMonad m => HiFun -> HiExpr -> HiExpr -> Evaluation m
biFunctionSet HiFunAnd a b = do
  x <- evalT a
  case x of
    HiValueBool False -> return x
    HiValueNull -> return x
    _ -> evalT b
biFunctionSet HiFunOr a b = do
  x <- evalT a
  case x of
    HiValueBool False -> evalT b
    HiValueNull -> evalT b
    _                  -> return x
biFunctionSet f a b = do
  x <- evalT a
  y <- evalT b
  biActiveFunctions f x y


teFunctionSet :: HiMonad m => HiFun -> HiExpr -> HiExpr -> HiExpr -> Evaluation m
teFunctionSet HiFunIf cond a b = do
  condition <- evalT cond
  case condition of
    (HiValueBool True) -> evalT a
    (HiValueBool False) -> evalT b
    _                   -> throwE HiErrorInvalidArgument
teFunctionSet _ _ _ _ = throwE HiErrorInvalidArgument

biActiveFunctions :: HiMonad m => HiFun -> HiValue -> HiValue -> Evaluation m
biActiveFunctions HiFunAdd (HiValueNumber a) (HiValueNumber b) = return $ HiValueNumber $ a + b
biActiveFunctions HiFunAdd (HiValueString a) (HiValueString b) = return $ HiValueString $ a <> b
biActiveFunctions HiFunAdd (HiValueList a) (HiValueList b) = return $ HiValueList $ a <> b
biActiveFunctions HiFunAdd (HiValueBytes a) (HiValueBytes b) = return $ HiValueBytes $ a <> b
biActiveFunctions HiFunAdd (HiValueTime a) (HiValueNumber b) = return $ HiValueTime $ addUTCTime (secondsToNominalDiffTime (fromRational b)) a

biActiveFunctions HiFunSub (HiValueNumber a) (HiValueNumber b) = return $ HiValueNumber $ a - b
biActiveFunctions HiFunSub (HiValueTime a) (HiValueTime b) = return $ HiValueNumber $ toRational $ diffUTCTime a b
biActiveFunctions HiFunMul (HiValueNumber a) (HiValueNumber b) = return $ HiValueNumber $ a * b
biActiveFunctions HiFunMul (HiValueString a) (HiValueNumber (n :% 1)) = return $ HiValueString $ stimes n a
biActiveFunctions HiFunMul (HiValueList a) (HiValueNumber (n :% 1)) = return $ HiValueList $ stimes n a
biActiveFunctions HiFunMul (HiValueBytes a) (HiValueNumber (n :% 1)) = return $ HiValueBytes $ stimes n a

biActiveFunctions HiFunDiv (HiValueNumber _) (HiValueNumber 0) = throwE HiErrorDivideByZero
biActiveFunctions HiFunDiv (HiValueNumber a) (HiValueNumber b) = return $ HiValueNumber $ a / b
biActiveFunctions HiFunDiv (HiValueString a) (HiValueString b) = return $ HiValueString $ a <> T.singleton '/' <> b

biActiveFunctions HiFunLessThan a b = return $ HiValueBool $ a < b
biActiveFunctions HiFunGreaterThan a b = return $ HiValueBool $ a > b
biActiveFunctions HiFunEquals a b = return $ HiValueBool $ a == b
biActiveFunctions HiFunNotLessThan a b = return $ HiValueBool $ a >= b
biActiveFunctions HiFunNotGreaterThan a b = return $ HiValueBool $ a <= b
biActiveFunctions HiFunNotEquals a b = return $ HiValueBool $ a /= b
biActiveFunctions HiFunRange (HiValueNumber a) (HiValueNumber b) = return $ HiValueList $ fromList $ HiValueNumber <$> [a,a+1..b]
biActiveFunctions HiFunFold (HiValueFunction _) (HiValueList Empty) = return HiValueNull
biActiveFunctions HiFunFold (HiValueFunction f) (HiValueList args@(_:<|_)) = case toArity f of
  Binary g -> foldl1 (\x y -> do
    a <- x
    b <- y
    biFunctionSet g (HiExprValue a) (HiExprValue b)) (return <$> args)
  _        -> throwE HiErrorInvalidArgument
biActiveFunctions HiFunWrite (HiValueString p) (HiValueString s) = return $ HiValueAction $ HiActionWrite (T.unpack p) (encodeUtf8 s)
biActiveFunctions HiFunRand (HiValueNumber (a :% 1)) (HiValueNumber (b :% 1)) = return $ HiValueAction $ on HiActionRand fromInteger  a b
biActiveFunctions _ _ _ = throwE HiErrorInvalidArgument


runOperation :: HiMonad m => HiValue -> ExceptT HiError m HiValue
runOperation (HiValueAction a) = ExceptT (Right <$> runAction a)
runOperation _ = throwE HiErrorInvalidArgument

toDict :: HiMonad m => [(HiExpr, HiExpr)] -> Evaluation m
toDict ls = HiValueDict <$> foldM (flip $ \(key, value) -> liftA3 insert (evalT key) (evalT value) . return) M.empty ls

evalT :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalT (HiExprValue number) = return number
evalT (HiExprApply f args) = do
  g <- evalT f
  case g of
    (HiValueFunction h) -> toOperation (toArity h) args
    (HiValueString s)   -> toOperation (toArity $ HiFunCallString s) args
    (HiValueList s)     -> toOperation (toArity $ HiFunCallList s) args
    (HiValueBytes s)    -> toOperation (toArity $ HiFunCallBytes s) args
    (HiValueDict d)     -> toOperation (toArity $ HiFunCallDict d) args
    _                   -> throwE HiErrorInvalidFunction
evalT (HiExprRun r) = runOperation =<< evalT r
evalT (HiExprDict r) = toDict r


eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalT