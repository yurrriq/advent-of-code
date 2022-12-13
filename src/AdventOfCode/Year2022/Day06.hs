module AdventOfCode.Year2022.Day06 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMainMaybe, inputFilePath)
import Data.List (findIndex, tails)
import qualified Data.Set as Set
import Text.Trifecta (letter, some)

main :: IO ()
main = $(defaultMainMaybe)

partOne :: [Char] -> Maybe Int
partOne = day06 4

partTwo :: [Char] -> Maybe Int
partTwo = day06 14

day06 :: Int -> [Char] -> Maybe Int
day06 n =
  fmap (n +)
    . findIndex ((n ==) . Set.size . Set.fromList)
    . map (take n)
    . tails

getInput :: IO [Char]
getInput = parseInput (some letter) $(inputFilePath)
