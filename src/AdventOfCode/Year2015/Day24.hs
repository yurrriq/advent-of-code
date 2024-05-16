module AdventOfCode.Year2015.Day24
  ( main,
    partOne,
    partTwo,
    getInput,
    example,
    bruteForce,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.List (minimumBy, subsequences)
import Data.Ord (comparing)
import Safe (findJust)
import Text.Trifecta (natural, some)

main :: IO ()
main = $(defaultMain)

partOne :: [Int] -> Int
partOne = balanceTheSleigh 3

partTwo :: [Int] -> Int
partTwo = balanceTheSleigh 4

getInput :: IO [Int]
getInput = parseInput (some (fromInteger <$> natural)) $(inputFilePath)

example :: [Int]
example = [1 .. 5] ++ [7 .. 11]

balanceTheSleigh :: Int -> [Int] -> Int
balanceTheSleigh numGroups packages =
  minimalArrangement (sum packages `div` numGroups) packages

minimalArrangement :: Int -> [Int] -> Int
minimalArrangement targetWeight weights =
  product
    . minimumBy
      (comparing length <> comparing product)
    . findJust (not . null)
    $ [arrangements n targetWeight weights | n <- [2 ..]]

arrangements :: Int -> Int -> [Int] -> [[Int]]
arrangements 0 _ _ = []
arrangements _ 0 [] = [[]]
arrangements _ _ [] = []
arrangements sizeLimit targetWeight (weight : weights)
  | targetWeight < 0 = []
  | otherwise = with ++ without
  where
    with =
      (weight :)
        <$> arrangements (sizeLimit - 1) (targetWeight - weight) weights
    without = arrangements sizeLimit targetWeight weights

bruteForce :: Int -> [Int] -> Int
bruteForce numGroups weights =
  product . minimumBy (comparing length <> comparing product) $
    filter ((== targetWeight) . sum) (subsequences weights)
  where
    targetWeight = sum weights `div` numGroups
