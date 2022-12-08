module AdventOfCode.Year2022.Day04 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.Ix (Ix, inRange)
import Text.Trifecta (Parser, char, comma, decimal, newline, sepEndBy)

main :: IO ()
main = $(defaultMain)

partOne :: [((Integer, Integer), (Integer, Integer))] -> Int
partOne = length . filter (\(ab, cd) -> ab `fullyContains` cd || cd `fullyContains` ab)

partTwo :: [((Integer, Integer), (Integer, Integer))] -> Int
partTwo = length . filter (\(ab, cd) -> ab `overlaps` cd || cd `overlaps` ab)

getInput :: IO [((Integer, Integer), (Integer, Integer))]
getInput = parseInput assignmentPairs $(inputFilePath)

assignmentPairs :: Parser [((Integer, Integer), (Integer, Integer))]
assignmentPairs = assignmentPair `sepEndBy` newline
  where
    assignmentPair = (,) <$> (assignment <* comma) <*> assignment
    assignment = (,) <$> (decimal <* char '-') <*> decimal

fullyContains :: Ix a => (a, a) -> (a, a) -> Bool
fullyContains (a, b) (c, d) = inRange (a, b) c && inRange (a, b) d

infix 4 `fullyContains`

overlaps :: Ix a => (a, a) -> (a, a) -> Bool
overlaps (a, b) (c, d) = inRange (a, b) c || inRange (a, b) d

infix 4 `overlaps`

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
