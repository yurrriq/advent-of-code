module AdventOfCode.Year2017.Day02 where

import AdventOfCode.Input
import AdventOfCode.TH
import Control.Arrow ((&&&))
import Data.List (foldl')
import Text.Trifecta

getInput :: IO [[Int]]
getInput = parseInput (some (row <* newline)) $(inputFilePath)
  where
    row = int `sepBy` tab
    int = read <$> some digit

partOne :: [[Int]] -> Int
partOne = foldl' go 0
  where
    go z = (z +) . uncurry (-) . (maximum &&& minimum)
