module AdventOfCode.Year2022.Day03 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Data.Char (ord)
import Data.List (intersect, nub)
import Data.Monoid (Sum (..), getSum)
import Text.Trifecta (Parser, lower, newline, sepEndBy, some, upper)

data Item
  = Lower Char
  | Upper Char
  deriving (Eq, Show)

main :: IO ()
main = $(defaultMain)

partOne :: [([Item], [Item])] -> Int
partOne =
  getSum . mconcat
    . foldMap
      ( map (Sum . prioritize)
          . uncurry intersect'
      )

partTwo :: [([Item], [Item])] -> Int
partTwo = undefined

example :: IO [([Item], [Item])]
example =
  parseString rucksacks . unlines $
    [ "vJrwpWtwJgWrhcsFMMfFFhFp",
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
      "PmmdzqPrVvPwwTWBwg",
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
      "ttgJtRGJQctTZtZT",
      "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]

getInput :: IO [([Item], [Item])]
getInput = parseInput rucksacks $(inputFilePath)

rucksacks :: Parser [([Item], [Item])]
rucksacks = rucksack `sepEndBy` newline
  where
    rucksack = halve <$> some item
    item = (Lower <$> lower) <|> (Upper <$> upper)

prioritize :: Item -> Int
prioritize (Lower c) = ord c - 96
prioritize (Upper c) = ord c - 38

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' xs ys = nub (xs `intersect` ys)
