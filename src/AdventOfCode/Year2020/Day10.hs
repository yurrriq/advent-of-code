module AdventOfCode.Year2020.Day10 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (frequencies)
import Data.HashMap.Strict ((!))
import Data.List (sort)
import Text.Trifecta (natural, some)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne input

-- putStr "Part Two: "
-- print $ partTwo input

getInput :: IO [Int]
getInput = sort <$> parseInput (some (fromInteger <$> natural)) $(inputFilePath)

-- getInput =
--   do
--     Just res <- parseFromFile (some (fromInteger <$> natural)) "../../../input/2020/day10.txt"
--     pure (sort res)

partOne :: [Int] -> Int
partOne adapters = (connections ! 1) * (connections ! 3)
  where
    connections = frequencies (zipWith (-) (tail adapters') adapters')
    adapters' = 0 : adapters ++ [maximum adapters + 3]

partTwo :: [Int] -> Int
partTwo = undefined
