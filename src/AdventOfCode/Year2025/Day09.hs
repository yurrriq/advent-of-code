{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day09 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.Foldable (maximum)
import Linear (V2 (..))
import Relude
import Text.Trifecta (Parser, comma, decimal, newline, sepEndBy)

locations :: Parser [V2 Integer]
locations = location `sepEndBy` newline
  where
    location = V2 <$> (decimal <* comma) <*> decimal

getExample :: IO [V2 Integer]
getExample = parseString locations example

example :: String
example =
  "7,1\n\
  \11,1\n\
  \11,7\n\
  \9,7\n\
  \9,5\n\
  \2,5\n\
  \2,3\n\
  \7,3"

getInput :: IO [V2 Integer]
getInput = parseInputAoC 2025 9 locations

-- allPairs :: (Metric f, Num a, Ord a) => [f a] -> [(f a, f a)]
allPairs :: [a] -> [(a, a)]
allPairs xs =
  -- sortOn (Down . (uncurry qd))
  [(p, q) | p : ps <- tails xs, q <- ps]

partOne :: SimplePuzzle [V2 Integer] Integer
partOne = asks (maximum . map area . allPairs)

area :: (V2 Integer, V2 Integer) -> Integer
area = product . (1 +) . abs . uncurry (-)

partTwo :: SimplePuzzle [V2 Integer] Integer
partTwo = fail "not yet implemented"

main :: IO ()
main = $(defaultMainPuzzle)
