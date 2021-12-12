module AdventOfCode.Year2021.Day07 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.List (sort)
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
partTwo = undefined

partOneCost :: Int -> Int -> Int
partOneCost x y = abs (x - y)

medianInt :: [Int] -> Int
medianInt xs = sort xs !! round (fromIntegral (length xs) / (2 :: Double))
