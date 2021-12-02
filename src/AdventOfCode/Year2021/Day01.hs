module AdventOfCode.Year2021.Day01 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (count)
import Data.List (tails)
import Text.Trifecta (many, natural)

main :: IO ()
main = $(defaultMain)

getInput :: IO [Integer]
getInput = parseInput (many natural) $(inputFilePath)

example :: [Integer]
example = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

partOne :: [Integer] -> Int
partOne xs = count (== LT) $ zipWith compare xs (tail xs)

partTwo :: [Integer] -> Int
partTwo =
  partOne
    . map sum
    . takeWhile ((>= 3) . length)
    . map (take 3)
    . tails
