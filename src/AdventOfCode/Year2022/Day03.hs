{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2022.Day03 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.SimplePuzzle
import AdventOfCode.TH (evalPuzzle)
import Data.List (foldl1, intersect)
import Data.List.Split (chunksOf)
import Relude
import Text.Trifecta (Parser, lower, newline, sepEndBy, upper)

main :: IO ()
main = $(evalPuzzle)

partOne :: SimplePuzzle [[Int]] Int
partOne = asks $ day03 (uncurry intersect . halve)
  where
    halve xs = splitAt (length xs `div` 2) xs

partTwo :: SimplePuzzle [[Int]] Int
partTwo = asks $ day03 (foldl1 intersect') . chunksOf 3
  where
    intersect' xs ys = ordNub (xs `intersect` ys)

day03 :: (a -> [Int]) -> [a] -> Int
day03 f = getSum . mconcat . foldMap (map Sum . take 1 . f)

getInput :: IO [[Int]]
getInput = parseInputAoC 2022 3 rucksacks

rucksacks :: Parser [[Int]]
rucksacks = some prioritizedItem `sepEndBy` newline
  where
    prioritizedItem = lowerItem <|> upperItem
    lowerItem = subtract 96 . ord <$> lower
    upperItem = subtract 38 . ord <$> upper

getExample :: IO [[Int]]
getExample = parseString rucksacks example

example :: String
example =
  "vJrwpWtwJgWrhcsFMMfFFhFp\n\
  \jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
  \PmmdzqPrVvPwwTWBwg\n\
  \wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
  \ttgJtRGJQctTZtZT\n\
  \CrZsJsPPZsGzwwsLwLmpwMDw\n"
