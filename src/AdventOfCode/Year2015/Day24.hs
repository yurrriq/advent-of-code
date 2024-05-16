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
partOne xs = minimalArrangement (sum xs `div` 3) xs

partTwo :: [Int] -> Int
partTwo = undefined

getInput :: IO [Int]
getInput = parseInput (some (fromInteger <$> natural)) $(inputFilePath)

example :: [Int]
example = [1 .. 5] ++ [7 .. 11]

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
