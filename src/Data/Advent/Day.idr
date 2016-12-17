-- ----------------------------------------------------------------- [ Day.idr ]
-- Module      : Data.Advent.Day
-- Description : Common structure for all puzzles, inspired by
--               Steve Purcell's Haskell solution.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : https://github.com/purcell/adventofcode2016
-- --------------------------------------------------------------------- [ EOH ]
||| Common Structure for All Puzzles
module Data.Advent.Day

import Debug.Error

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

%default partial

%access public export

record Day (i : Type) where
    constructor MkDay
    dayNum     : Nat
    dayParser  : Parser i
    dayPartOne : i -> IO String
    dayPartTwo : i -> IO String

%access export

loadDay : Day i -> IO i
loadDay d = either error pure $
            either (Left . show) (parse (dayParser d <* eof))
            !(readFile $ "input/day" ++ zeroPad (dayNum d) ++ ".txt")
  where
    zeroPad : Nat -> String
    zeroPad n = if n < 10 then strCons '0' (show n) else show n

runDay : Day i -> IO ()
runDay d = do args <- getArgs
              input <- loadDay d
              when (isNil args || ("one" `elem` args)) $
                   do putStr $ banner "One"
                      putStrLn !(dayPartOne d input)
              when (isNil args || ("two" `elem` args)) $
                   do putStr $ banner "Two"
                      putStrLn !(dayPartTwo d input)
  where
    banner : String -> String
    banner part = "Day " ++ show (dayNum d) ++ ": Part " ++ part ++ " ==> "

-- --------------------------------------------------------------------- [ EOF ]
