module AdventOfCode.Year2022.Day04 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative (liftA2)
import Data.Ix (inRange)
import Data.Tuple (swap)
import Text.Trifecta (Parser, char, comma, decimal, newline, sepEndBy)

main :: IO ()
main = $(defaultMain)

partOne :: [((Integer, Integer), (Integer, Integer))] -> Int
partOne = day03 fullyContains
  where
    fullyContains (ab, (c, d)) = inRange ab c && inRange ab d

partTwo :: [((Integer, Integer), (Integer, Integer))] -> Int
partTwo = day03 overlaps
  where
    overlaps (ab, (c, d)) = inRange ab c || inRange ab d

day03 :: ((a, a) -> Bool) -> [(a, a)] -> Int
day03 f = length . filter (liftA2 (||) f (f . swap))

getInput :: IO [((Integer, Integer), (Integer, Integer))]
getInput = parseInput assignmentPairs $(inputFilePath)

assignmentPairs :: Parser [((Integer, Integer), (Integer, Integer))]
assignmentPairs = assignmentPair `sepEndBy` newline
  where
    assignmentPair = (,) <$> (assignment <* comma) <*> assignment
    assignment = (,) <$> (decimal <* char '-') <*> decimal

example :: IO [((Integer, Integer), (Integer, Integer))]
example =
  parseString assignmentPairs . unlines $
    [ "2-4,6-8",
      "2-3,4-5",
      "5-7,7-9",
      "2-8,3-7",
      "6-6,4-6",
      "2-6,4-8"
    ]
