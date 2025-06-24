module AdventOfCode.Year2021.Day07 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (frequenciesInt)
import Control.Arrow ((&&&))
import Data.IntMap qualified as IM
import Data.List (maximumBy, sort)
import Data.Ord (comparing)
import Text.Trifecta (commaSep, natural)

main :: IO ()
main = $(defaultMain)

getInput :: IO [Int]
getInput = parseInput (map fromInteger <$> commaSep natural) $(inputFilePath)

example :: [Int]
example = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

partOne :: [Int] -> Int
partOne xs = sum (map (partOneCost (medianInt xs)) xs)

partTwo :: [Int] -> Int
partTwo xs = go (meanInt xs)
  where
    go x
      | predCost < xCost = go predX
      | succCost < xCost = go succX
      | otherwise = xCost
      where
        xCost = cost x
        (predCost, succCost) = (cost predX, cost succX)
        (predX, succX) = (pred &&& succ) x
        cost = sum . flip map xs . partTwoCost

partOneCost :: Int -> Int -> Int
partOneCost x y = abs (x - y)

partTwoCost :: Int -> Int -> Int
partTwoCost x y = sumOneToN (partOneCost x y)

medianInt :: [Int] -> Int
medianInt xs = sort xs !! round (fromIntegral (length xs) / (2 :: Double))

meanInt :: [Int] -> Int
meanInt = fst . maximumBy (comparing snd) . IM.toList . frequenciesInt

sumOneToN :: Int -> Int
sumOneToN n = (n * (n + 1)) `div` 2
