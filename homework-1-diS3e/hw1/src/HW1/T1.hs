{-# LANGUAGE LambdaCase #-}


module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

nextDay :: Day -> Day
nextDay = \case
    Monday    -> Tuesday
    Tuesday   -> Wednesday
    Wednesday -> Thursday
    Thursday  -> Friday
    Friday    -> Saturday
    Saturday  -> Sunday
    Sunday    -> Monday


afterDays :: Natural -> Day -> Day
afterDays 0 = id
afterDays n = nextDay . (afterDays . subtract 1) n

isWeekend :: Day -> Bool
isWeekend = \case
   Saturday -> True
   Sunday   -> True
   _        -> False

daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty day =  (+) 1 . daysToParty $ nextDay day 
