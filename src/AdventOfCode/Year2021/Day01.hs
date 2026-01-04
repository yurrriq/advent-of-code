{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2021.Day01 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (count)
import Relude
import Text.Trifecta (natural)

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO [Integer]
getInput = parseInputAoC 2021 1 (some natural)

getExample :: IO [Integer]
getExample = pure [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

partOne :: SimplePuzzle [Integer] Int
partOne = asks (countPairwiseIncreases 1)

partTwo :: SimplePuzzle [Integer] Int
partTwo = asks (countPairwiseIncreases 3)

countPairwiseIncreases :: (Ord a) => Int -> [a] -> Int
countPairwiseIncreases n =
  count (== LT)
    . uncurry (zipWith compare)
    . (id &&& drop n)
