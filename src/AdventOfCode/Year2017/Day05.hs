{-# LANGUAGE TypeOperators #-}

module AdventOfCode.Year2017.Day05 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Lens (over, view)
import Control.Monad (ap)
import Control.Zipper
import Data.List (unfoldr)
import Data.Tuple.Extra (dupe)
import Text.Trifecta (integer, some)

type CPU = Top :>> [Int] :>> Int

main :: IO ()
main = $(defaultMain)

getInput :: IO [Int]
getInput = parseInput (some (fromInteger <$> integer)) $(inputFilePath)

example :: [Int]
example = [0, 3, 0, 1, -3]

partOne :: [Int] -> Int
partOne = solve (+ 1)

partTwo :: [Int] -> Int
partTwo = solve updateOffset
  where
    updateOffset offset
      | offset >= 3 = offset - 1
      | otherwise = offset + 1

solve :: (Int -> Int) -> [Int] -> Int
solve f = length . iterateMaybe (step f) . (fromWithin traverse . zipper)

step :: (Int -> Int) -> CPU -> Maybe CPU
step = ap (move . view focus) . over focus

move :: Int -> CPU -> Maybe CPU
move n = case compare n 0 of
  LT -> jerks leftward (abs n)
  EQ -> Just
  GT -> jerks rightward n

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : unfoldr (fmap dupe . f) x
