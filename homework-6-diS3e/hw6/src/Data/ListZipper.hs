-- | This module defines 'ListZipper' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.ListZipper
  ( ListZipper (..)
  , lRight
  , lLeft
  , lGenerator
  , lWrite
  , toList
  ) where

import Control.Comonad (Comonad (..))

data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ l a r) = LZ (fmap f l) (f a) (fmap f r)

instance Comonad ListZipper where
  extract (LZ _ a _) = a

  duplicate = lGenerator lLeft lRight

lLeft :: ListZipper a -> ListZipper a
lLeft (LZ (l : ls) a r) = LZ ls l (a : r)
lLeft emptyLeft = emptyLeft

lRight :: ListZipper a -> ListZipper a
lRight (LZ l a (r : rs)) = LZ (a : l) r rs
lRight emptyRight = emptyRight

lGenerator :: (a -> a) -> (a -> a) -> a -> ListZipper a
lGenerator f g x = LZ (iterateTail f x) x (iterateTail g x)

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

lWrite :: a -> ListZipper a -> ListZipper a
lWrite v (LZ l _ r) = LZ l v r

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs