{-# LANGUAGE LambdaCase #-}
module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid

  , simulate
  ) where

import System.Random (StdGen, mkStdGen, uniformR, Random (randoms))

import Data.Grid
import Data.ListZipper
import Control.Comonad
import Control.Monad (liftM2)
import Data.Foldable

data Config = Config
  { probability :: Double
  , incubationPeriod :: Int
  , illnessDuration :: Int
  , immunityDuration :: Int
  } deriving Show

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int

instance Show CellState where
  show = \case
    Healthy -> "_"
    Infected _ -> "i"
    Ill _ -> "#"
    Immune _ -> "@"


data Cell = Cell
  { cellState :: CellState
  , cellRand :: StdGen
  }

instance Show Cell where
  show (Cell s _) = show s

type Comonad19Grid = Grid Cell

simulate :: Int -> Config -> [Comonad19Grid]
simulate seed cfg = iterate (simulationStep cfg) (startState seed cfg)

startState :: Int -> Config -> Comonad19Grid
startState seed cfg = setState (Infected $ incubationPeriod cfg) $ emptyGrid (Cell Healthy . mkStdGen)
  where emptyGrid f =
          let ls = f <$> randoms (mkStdGen (-seed))
              rs = f <$> randoms (mkStdGen seed) in
            Grid $ duplicate $ LZ  ls (f seed) rs

setState :: CellState -> Comonad19Grid -> Comonad19Grid
setState s g = gWrite (Cell s (cellRand $ extract g)) g

simulationStep :: Config -> Comonad19Grid -> Comonad19Grid
simulationStep cfg = extend (rule cfg)

neighbors :: [Grid a -> Grid a]
neighbors = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [gLeft, gRight]
        verticals   = [gUp, gDown]

rule :: Config -> Comonad19Grid -> Cell
rule cfg g =
  let cell = extract g in
    case cellState cell of
      Healthy -> 
        let (infected, generator) = foldl' (
              \st@(ans, gen) neighbor -> case neighbor of
              Healthy -> st
              Immune _ -> st
              _ -> let (v, newGen) = uniformR (0 :: Double, 1 :: Double) gen in (ans || (v < probability cfg), newGen)
              ) (False, cellRand cell) (fmap (\direction -> cellState $ extract $ direction g) neighbors) in 
                if infected 
                  then Cell (Infected $ incubationPeriod cfg) generator 
                  else Cell Healthy generator
      Infected 1 -> Cell (Ill $ illnessDuration cfg) (cellRand cell)
      Infected n -> Cell (Infected $ n - 1) (cellRand cell)
      Ill 1 -> Cell (Immune $ immunityDuration cfg) (cellRand cell)
      Ill n -> Cell (Ill $ n - 1) (cellRand cell)
      Immune 1 -> Cell Healthy (cellRand cell)
      Immune n -> Cell (Immune $ n - 1) (cellRand cell)
