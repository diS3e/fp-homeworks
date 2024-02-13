module Main (main) where

import Options.Applicative
import Control.Exception (throwIO, Exception)
import Control.Monad
import HW6.T3
import Data.ListZipper (ListZipper (..), toList)
import Data.Grid

data Opts = Opts
    { optProb :: !Double
    , optIncub :: !Int
    , optIll :: !Int
    , optImmun :: !Int
    , optGridSize :: !Int
    , optIterations :: !Int
    }

data OptsException =
    IncorrectProbability
    | IncorrectIncubation
    | IncorrectIll
    | IncorrectImmun
    | IncorrectGridSize
    | IncorrectIterations
    deriving Show

instance Exception OptsException
createConfig :: Opts -> IO (Config, Int, Int)
createConfig opts = do
    let prob = optProb opts
        incub = optIncub opts
        ill = optIll opts
        immun = optImmun opts
        gridSize = optGridSize opts
        iters = optIterations opts
    unless (0 <= prob && prob <= 1) (throwIO IncorrectProbability)
    unless (0 <= incub) (throwIO IncorrectIncubation)
    unless (0 <= ill) (throwIO IncorrectIll)
    unless (0 <= immun) (throwIO IncorrectImmun)
    unless (0 <= gridSize && gridSize `mod` 2 == 1) (throwIO IncorrectGridSize)
    unless (0 <= iters) (throwIO IncorrectIterations)
    return (Config prob incub ill immun, gridSize, iters)

defaultSeed :: Int
defaultSeed = 238

showCellLine :: Int -> ListZipper Cell -> IO ()
showCellLine size lz = putStrLn $ show =<< toList lz size

showGrid :: Int -> Comonad19Grid -> IO ()
showGrid size (Grid lz) = mapM_ (showCellLine size) (toList lz size)


main :: IO ()
main = do
    opts <- execParser optsParser
    (cfg, size, iters) <- createConfig opts
    let halfSize = (size - 1) `div` 2
    let array = take iters $ simulate defaultSeed cfg
    mapM_ (\x -> showGrid halfSize x >> putStrLn "") array
  where
    optsParser :: ParserInfo Opts
    optsParser =
        info
            (helper <*> programOptions)
            (fullDesc <> progDesc "Example: stack exec -- comonad19 --prob 0.2 --incub 2 --ill 5 --immun 7 --grid-size 11 --iterations 10" <>
             header
                 "\"Comonad19\" simulation of the dissemination of ideas of functional programming among CT students.")

    programOptions :: Parser Opts
    programOptions = Opts <$>
            ((read :: String -> Double) <$> strOption (
                long "prob"
                <> value "0.5"
                <> help "Infection probability"
                ))
        <*> ((read :: String -> Int) <$> strOption (
                long "incub"
                <> value "2"
                <> help "Incubation period duration"))
        <*> ((read :: String -> Int) <$> strOption (
                long "ill"
                <> value "5"
                <> help "Illness duration"
                ))
        <*> ((read :: String -> Int) <$> strOption (
                long "immun"
                <> value "7"
                <> help "Immunity duration"
                ))
        <*> ((read :: String -> Int) <$> strOption (
                long "grid-size"
                <> value "11"
                <> help "Output grid size"
                ))
        <*> ((read :: String -> Int) <$> strOption (
                long "iterations"
                <> value "10"
                <> help "The number of simulation iterations"
                ))
