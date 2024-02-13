module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ initial Leaf = initial
tfoldr f initial (Branch _ left root right) =  tfoldr f (f root $ tfoldr f initial right) left
