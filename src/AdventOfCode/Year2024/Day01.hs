module AdventOfCode.Year2024.Day01 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (frequencies)
import Data.List (sort, transpose)
import Data.List.Extra (sumOn')
import qualified Data.Map as Map
import Text.Trifecta (count, natural, some)

main :: IO ()
main = $(defaultMain)

partOne :: (Num a, Ord a) => ([a], [a]) -> a
partOne (xs, ys) = sumOn' abs $ zipWith subtract (sort xs) (sort ys)

partTwo :: (Num a, Ord a) => ([a], [a]) -> a
partTwo (xs, ys) = sumOn' go xs
  where
    go x = x * fromIntegral (Map.findWithDefault 0 x (frequencies ys))

getInput :: IO ([Integer], [Integer])
getInput =
  do
    [xs, ys] <-
      transpose
        <$> parseInput (some (count 2 natural)) $(inputFilePath)
    pure (xs, ys)
