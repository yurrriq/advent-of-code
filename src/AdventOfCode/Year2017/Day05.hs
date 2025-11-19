{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2017.Day05 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.SimplePuzzle
import AdventOfCode.TH (evalPuzzle)
import AdventOfCode.Util (iterateMaybe)
import Control.Lens (over, views)
import Control.Monad (ap)
import Control.Zipper
import Relude
import Text.Trifecta (integer)

type CPU = Top :>> [Int] :>> Int

main :: IO ()
main = $(evalPuzzle)

getExample :: IO [Int]
getExample = pure [0, 3, 0, 1, -3]

getInput :: IO [Int]
getInput = parseInputAoC 2017 5 (some (fromInteger <$> integer))

partOne :: SimplePuzzle [Int] Int
partOne = asks (solve (+ 1))

partTwo :: SimplePuzzle [Int] Int
partTwo = asks (solve updateOffset)
  where
    updateOffset offset
      | offset >= 3 = offset - 1
      | otherwise = offset + 1

solve :: (Int -> Int) -> [Int] -> Int
solve f = length . iterateMaybe (step f) . fromWithin traverse . zipper

step :: (Int -> Int) -> CPU -> Maybe CPU
step f = views focus move `ap` over focus f

move :: Int -> CPU -> Maybe CPU
move n = case compare n 0 of
  LT -> jerks leftward (abs n)
  EQ -> Just
  GT -> jerks rightward n
