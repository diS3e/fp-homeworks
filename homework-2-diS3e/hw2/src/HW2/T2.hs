module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = [] :| []
splitOn separator (x:xs) =
  let (y :| ys) = splitOn separator xs in
    if x == separator
      then [] :| (y : ys)
      else (x : y) :| ys


joinWith :: a -> NonEmpty [a] -> [a]
joinWith separator = foldr1 $ flip $ flip (++) . (:) separator
