module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Size   = Int
type Depth = Int
type Meta = (Depth, Size)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                     = 0
tsize (Branch (_, size) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf                       = 0
tdepth (Branch (height, _) _ _ _) = height

diff :: Tree a -> Int
diff Leaf = undefined
diff (Branch _ left _ right) = tdepth left - tdepth right


mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left root right =
    let recount extractor combiner = (+) 1 $ extractor left `combiner` extractor right
    in  Branch (recount tdepth max, recount tsize (+)) left root right

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember a (Branch _ left root right)
      | a < root = tmember a left
      | a == root = True
      | a > root = tmember a right


smallLeftRotate :: Tree a -> Tree a
smallLeftRotate (Branch _ p a (Branch _ q b r)) = mkBranch (mkBranch p a q) b r

smallRightRotate :: Tree a -> Tree a
smallRightRotate (Branch _ (Branch _ p a q) b r) = mkBranch p a $ mkBranch q b r

bigLeftRotate :: Tree a -> Tree a
bigLeftRotate (Branch _ left a right) = smallLeftRotate $ mkBranch left a $ smallRightRotate right

bigRightRotate :: Tree a -> Tree a
bigRightRotate (Branch _ left a right) = smallRightRotate $ mkBranch (smallLeftRotate left) a right

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance tree@(Branch _ left root right) 
             | diff tree == -2 = if diff right > 0 then bigLeftRotate tree else smallLeftRotate tree
             | diff tree == 2 = if diff left < 0 then bigRightRotate tree else smallRightRotate tree
             | otherwise = tree

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf     = mkBranch Leaf a Leaf
tinsert a tree@(Branch (height, size) left root right)
       | a < root  = balance $ mkBranch (tinsert a left) root right
       | a == root = tree
       | a > root  = balance $ mkBranch left root $ tinsert a right


tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf