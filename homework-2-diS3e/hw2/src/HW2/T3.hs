module HW2.T3
  ( epart
  , mcat
  ) where

import Data.Foldable 

fromMaybe :: Monoid a => Maybe a -> a
fromMaybe Nothing  = mempty
fromMaybe (Just x) = x

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap' fromMaybe

fromEither :: (Monoid a, Monoid b) => Either a b -> (a, b)
fromEither (Left a) = (a, mempty)
fromEither (Right b) = (mempty, b)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap' fromEither
