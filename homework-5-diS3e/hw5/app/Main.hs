module Main (main) where

import HW5.Parser (parse)
import HW5.Evaluator (eval)
import HW5.Pretty (prettyValue)
import Data.Set
import HW5.Action

main :: IO ()
main = do
    line <- getLine
    case parse line of
        (Left parseError) -> print parseError
        (Right expr) -> do
            result <- runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime])
            case result of
                (Left evalError) -> print evalError
                (Right value) -> print $ prettyValue value
    main
