module AdventOfCode.Year2024.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (count, holes)
import Data.Ix (inRange)
import Text.Trifecta (char, decimal, newline, sepBy1, sepEndBy)

main :: IO ()
main = $(defaultMain)

partOne :: [[Integer]] -> Int
partOne = count isSafe

partTwo :: [[Integer]] -> Int
partTwo = count (any isSafe) . map (map snd . holes)

getInput :: IO [[Integer]]
getInput = parseInput ((decimal `sepBy1` char ' ') `sepEndBy` newline) $(inputFilePath)

example :: [[Integer]]
example =
  [ [7, 6, 4, 2, 1],
    [1, 2, 7, 8, 9],
    [9, 7, 6, 2, 1],
    [1, 3, 2, 4, 5],
    [8, 6, 4, 4, 1],
    [1, 3, 6, 7, 9]
  ]

isSafe :: [Integer] -> Bool
isSafe [] = False
isSafe xxs@(_ : xs) = any (all (inRange (1, 3))) [diffs, negate <$> diffs]
  where
    diffs = zipWith subtract xxs xs
