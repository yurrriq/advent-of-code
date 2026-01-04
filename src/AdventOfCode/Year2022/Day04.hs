{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2022.Day04 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.Ix (inRange)
import Relude
import Text.Trifecta (Parser, char, comma, decimal, newline, sepEndBy)

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle [((Integer, Integer), (Integer, Integer))] Int
partOne = asks (solveWith fullyContains)
  where
    fullyContains (ab, (c, d)) = inRange ab c && inRange ab d

partTwo :: SimplePuzzle [((Integer, Integer), (Integer, Integer))] Int
partTwo = asks (solveWith overlaps)
  where
    overlaps (ab, (c, d)) = inRange ab c || inRange ab d

solveWith :: ((a, a) -> Bool) -> [(a, a)] -> Int
solveWith f = length . filter (liftA2 (||) f (f . swap))

getInput :: IO [((Integer, Integer), (Integer, Integer))]
getInput = parseInputAoC 2022 4 assignmentPairs

assignmentPairs :: Parser [((Integer, Integer), (Integer, Integer))]
assignmentPairs = assignmentPair `sepEndBy` newline
  where
    assignmentPair = (,) <$> (assignment <* comma) <*> assignment
    assignment = (,) <$> (decimal <* char '-') <*> decimal

getExample :: IO [((Integer, Integer), (Integer, Integer))]
getExample = parseString assignmentPairs example

example :: String
example =
  "2-4,6-8\n\
  \2-3,4-5\n\
  \5-7,7-9\n\
  \2-8,3-7\n\
  \6-6,4-6\n\
  \2-6,4-8\n"
