{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2022.Day06 where

import AdventOfCode.Input (rawInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (maybeFail)
import Data.List (findIndex)
import Data.Set qualified as Set
import Relude

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle String Int
partOne = ask >>= maybeFail "no start-of-packet marker detected" . day06 4

partTwo :: SimplePuzzle String Int
partTwo = ask >>= maybeFail "no start-of-message marker detected" . day06 14

day06 :: Int -> String -> Maybe Int
day06 n =
  fmap (n +)
    . findIndex ((n ==) . Set.size . Set.fromList)
    . map (take n)
    . tails

getInput :: IO String
getInput = rawInputAoC 2022 6
