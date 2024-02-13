module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural
import Data.Maybe

data N = Z | S N

nplus :: N -> N -> N
nplus a Z     = a
nplus a (S b) = S $ nplus a b

nmult :: N -> N -> N
nmult _ Z     = Z
nmult a (S b) = nplus a $ nmult a b

nsub :: N -> N -> Maybe N
nsub a Z         = Just a
nsub Z (S _)     = Nothing
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering
ncmp Z Z         = EQ
ncmp Z _         = LT
ncmp _ Z         = GT
ncmp (S a) (S b) = ncmp a b

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural $ (-) n 1

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S a) = (+) 1 $ nToNum a

nEven :: N -> Bool
nEven Z     = True
nEven (S Z) = False
nEven (S a) = not $ nEven a

nOdd :: N -> Bool
nOdd = not . nEven

ndiv :: N -> N -> N
ndiv _ Z                        = undefined
ndiv a b | isNothing $ nsub a b = Z
         | otherwise            = S $ ndiv  (fromJust $ nsub a b) b

nmod :: N -> N -> N
nmod _ Z                  = undefined
nmod a b | ncmp a b == LT = a
         | otherwise      = nmod (fromJust $ nsub a b) b
