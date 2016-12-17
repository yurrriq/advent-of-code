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

zeroPad : Nat -> String
zeroPad n = if n < 10
               then strCons '0' (show n)
               else show n

%access private

inputFile : Day i -> String
inputFile d = "input/day" ++ zeroPad (dayNum d) ++ ".txt"

banner : Day i -> String -> String
banner d part = "Day " ++ show (dayNum d) ++ ": Part " ++ part ++ " ==> "

%access export

loadDay : Day i -> IO i
loadDay d = either error pure $
            either (Left . show) (parse (dayParser d <* eof))
            !(readFile (inputFile d))

runDay : Day i -> IO ()
runDay d = do input <- loadDay d
              putStr $ banner d "One"
              putStrLn !(dayPartOne d input)
              putStr $ banner d "Two"
              putStrLn !(dayPartTwo d input)

-- --------------------------------------------------------------------- [ EOF ]
