module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Numeric.Natural (Natural)
import Data.Function (fix)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\g arr -> if null arr then [] else f (head arr) : g (tail arr))

fib :: Natural -> Natural
fib x = fibs !! fromIntegral x
  where fibs = fix (scanl (+) 0 . (1:))

fac :: Natural -> Natural
fac = fix (\f n -> if n == 0 then 1 else n * f (n - 1) )
