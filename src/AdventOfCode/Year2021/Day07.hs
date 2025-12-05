{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2021.Day07 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (medianUnsafe)
import Relude
import Text.Trifecta (commaSep, natural)

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO [Int]
getInput = parseInputAoC 2021 7 (map fromInteger <$> commaSep natural)

example :: [Int]
example = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

partOne :: SimplePuzzle [Int] Int
partOne = asks (\xs -> sum (map (partOneCost (medianUnsafe xs)) xs))

partTwo :: SimplePuzzle [Int] Int
partTwo = asks $ \xs ->
  let cost = sum . flip map xs . partTwoCost
      go x =
        if
          | cost (x - 1) < cost x -> go (x - 1)
          | cost (x + 1) < cost x -> go (x + 1)
          | otherwise -> cost x
   in go (medianUnsafe xs)

partOneCost :: Int -> Int -> Int
partOneCost x y = abs (x - y)

partTwoCost :: Int -> Int -> Int
partTwoCost x y = sumOneToN (partOneCost x y)

sumOneToN :: Int -> Int
sumOneToN n = (n * (n + 1)) `div` 2
