module AdventOfCode.Year2024.Day01 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.List (sort, transpose)
import Text.Trifecta (count, natural, some)

main :: IO ()
main = $(defaultMain)

partOne :: (Num a, Ord a) => [[a]] -> a
partOne [xs, ys] = sum $ zipWith ((abs .) . (-)) (sort xs) (sort ys)
partOne _ = error "Ope!"

partTwo :: (Num a, Ord a) => [[a]] -> a
partTwo = undefined

getInput :: IO [[Integer]]
getInput = transpose <$> parseInput (some (count 2 natural)) $(inputFilePath)
