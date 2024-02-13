{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module HW6.T1
  ( BucketsArray
  , CHT (..)

  , newCHT
  , getCHT
  , putCHT
  , sizeCHT

  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (STM, MonadConc (atomically), MonadSTM (newTVar, readTVar, writeTVar))
import Control.Concurrent.Classy.STM (TArray, TVar)
import Data.Hashable
import Data.Array.MArray (readArray, MArray (newArray, getBounds), writeArray, getElems)
import Data.Foldable (find)
import Control.Monad (when, join)
import GHC.Float (int2Double)

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }


newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = atomically $ do
  size <- newTVar 0
  array <- newArray (0, initCapacity - 1) []
  buckets <- newTVar array
  return $ CHT buckets size

getCHT
  :: ( MonadConc m
     , Eq k
     , Hashable k
     )
  => k
  -> CHT (STM m) k v
  -> m (Maybe v)
getCHT key table = atomically $ do
    buckets <- readTVar $ chtBuckets table
    range <- getBounds buckets
    bucket <- readArray buckets (hash key `mod` (snd range + 1))
    return $ snd <$> find ((== key) . fst) bucket

putCHT
  :: ( MonadConc m
     , Eq k
     , Hashable k)
  => k
  -> v
  -> CHT (STM m) k v
  -> m ()
putCHT key value table = atomically $ do
  buckets <- readTVar $ chtBuckets table
  size <- readTVar $ chtSize table
  range <- getBounds buckets
  let capacity = snd range + 1
  let h = hash key `mod` capacity
  bucket <- readArray buckets h
  case snd <$> find ((== key) . fst) bucket of
    Nothing -> do
      writeArray buckets h ((key, value) : bucket)
      writeTVar (chtSize table) (size + 1)
    Just _  -> return ()
  when (size >= truncate (int2Double capacity * loadFactor)) (do
    elems <- getElems buckets
    let allElems = join elems
        newCapacity = 2 * capacity
    new <- newArray (0, newCapacity - 1) []
    mapM_ (\r@(k, _) -> (do
      let newH = hash k `mod` newCapacity
      b <- readArray new newH
      writeArray new newH (r : b))) allElems
    writeTVar (chtBuckets table) new
    )

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT = atomically . readTVar . chtSize