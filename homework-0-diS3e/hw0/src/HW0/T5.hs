module HW0.T5
  ( Nat
  , nFromNatural
  , nmult
  , nplus
  , ns
  , nToNum
  , nz
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ = id

ns :: Nat a -> Nat a
ns n f z = f $ n f z  

nplus :: Nat a -> Nat a -> Nat a
nplus n s f z = n f $ s f z 

nmult :: Nat a -> Nat a -> Nat a
nmult n s f = n (s f)

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns $ nFromNatural $ (-) n 1

nToNum :: Num a => Nat a -> a
nToNum n = n (+ 1) 0  
