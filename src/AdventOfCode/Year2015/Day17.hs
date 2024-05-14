module AdventOfCode.Year2015.Day17
  ( main,
    partOne,
    partTwo,
    getInput,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.List (group, sort, subsequences)
import Text.Trifecta (integer, some)

main :: IO ()
main = $(defaultMain)

partOne :: [Int] -> Int
partOne = length . solutions

partTwo :: [Int] -> Int
partTwo =
  minimum
    . map length
    . group
    . sort
    . map length
    . solutions

getInput :: IO [Int]
getInput = parseInput (some (fromInteger <$> integer)) $(inputFilePath)

solutions :: [Int] -> [[Int]]
solutions = filter ((150 ==) . sum) . subsequences
