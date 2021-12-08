module AdventOfCode.Year2021.Day06 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Text.Trifecta (commaSep, natural)

main :: IO ()
main = $(defaultMain)

getInput :: IO [Int]
getInput = parseInput (commaSep (fromInteger <$> natural)) $(inputFilePath)

example :: [Int]
example = [3, 4, 3, 1, 2]

partOne :: [Int] -> Int
partOne = simulate 80

partTwo :: [Int] -> Int
partTwo = undefined

simulate :: Int -> [Int] -> Int
simulate n = length . (!! n) . iterate (concatMap step)
  where
    step 0 = [6, 8]
    step t = [t - 1]
