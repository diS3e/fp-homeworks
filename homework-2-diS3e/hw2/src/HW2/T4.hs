module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last a) b = a :+ b
  (<>) (x :+ xs) b = x :+ (<>) xs b

data Inclusive a b = This a | That b | Both a b
  deriving Show

-- You may necessary constraints there
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This a <> This b = This $ a <> b
  This a <> That b = Both a b
  This a <> Both b c = Both (a <> b) c
  That a <> This b =  Both b a
  That a <> That b = That $ a <> b
  That a <> Both b c = Both b (a <> c)
  Both a b <> This c =  Both (a <> c) b
  Both a b <> That c =  Both a (b <> c)
  Both a b <> Both c d = Both (a <> c) (b <> d)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  (<>) (DS x) (DS y) = DS $ x ++ separator ++ y
    where separator = if x == "" || y == "" then "" else "."

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F x) (F y) = F $ (.) x y

instance Monoid (Fun a) where
  mempty = F id
