module AdventOfCode.Year2021.Day01 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (count)
import Control.Arrow ((&&&))
import Text.Trifecta (natural, some)

main :: IO ()
main = $(defaultMain)

getInput :: IO [Integer]
getInput = parseInput (some natural) $(inputFilePath)

example :: [Integer]
example = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

partOne :: [Integer] -> Int
partOne = countPairwiseIncreases 1

partTwo :: [Integer] -> Int
partTwo = countPairwiseIncreases 3

countPairwiseIncreases :: (Ord a) => Int -> [a] -> Int
countPairwiseIncreases n =
  count (== LT)
    . uncurry (zipWith compare)
    . (id &&& drop n)
