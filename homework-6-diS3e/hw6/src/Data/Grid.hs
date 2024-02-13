-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..)
  , gWrite
  , gLeft
  , gRight
  , gUp
  , gDown
  ) where

import Control.Comonad (Comonad (..))

import Data.ListZipper

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Functor Grid where
  fmap f (Grid a) = Grid $ fmap (fmap f) a

instance Comonad Grid where
  extract = extract . extract . unGrid

  duplicate = Grid . fmap gHorizontal . gVertical


gLeft, gRight :: Grid a -> Grid a
gLeft (Grid a) = Grid (fmap lLeft a)
gRight (Grid a) = Grid (fmap lRight a)

gUp, gDown :: Grid a -> Grid a
gUp (Grid g) = Grid (lLeft g)
gDown (Grid g) = Grid (lRight g)

gHorizontal, gVertical :: Grid a -> ListZipper (Grid a)
gHorizontal = lGenerator gLeft gRight
gVertical = lGenerator gUp gDown

gWrite :: a -> Grid a -> Grid a
gWrite v (Grid g) = Grid $ lWrite (lWrite v $ extract g) g