module AdventOfCode.Year2022.Day03 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Data.Char (ord)
import Data.List (intersect, nub)
import Data.List.Split (chunksOf)
import Data.Monoid (Sum (..), getSum)
import Text.Trifecta (Parser, lower, newline, sepEndBy, some, upper)

main :: IO ()
main = $(defaultMain)

partOne :: [[Int]] -> Int
partOne = day03 (uncurry intersect . halve)
  where
    halve xs = splitAt (length xs `div` 2) xs

partTwo :: [[Int]] -> Int
partTwo = day03 (foldl1 intersect') . chunksOf 3
  where
    intersect' xs ys = nub (xs `intersect` ys)

day03 :: (a -> [Int]) -> [a] -> Int
day03 f = getSum . mconcat . foldMap (map Sum . take 1 . f)

getInput :: IO [[Int]]
getInput = parseInput rucksacks $(inputFilePath)

rucksacks :: Parser [[Int]]
rucksacks = some prioritizedItem `sepEndBy` newline
  where
    prioritizedItem = lowerItem <|> upperItem
    lowerItem = subtract 96 . ord <$> lower
    upperItem = subtract 38 . ord <$> upper

example :: IO [[Int]]
example =
  parseString rucksacks . unlines $
    [ "vJrwpWtwJgWrhcsFMMfFFhFp",
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
      "PmmdzqPrVvPwwTWBwg",
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
      "ttgJtRGJQctTZtZT",
      "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]
