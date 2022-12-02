module AdventOfCode.Year2022.Day01
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Text.Trifecta hiding (parseString)

main :: IO ()
main = $(defaultMain)

getInput :: IO [[Integer]]
getInput = parseInput inventory $(inputFilePath)

partOne :: [[Integer]] -> Integer
partOne = maximum . map sum

partTwo :: [[Integer]] -> Integer
partTwo = sum . take 3 . sortBy (comparing Down) . map sum

inventory :: Parser [[Integer]]
inventory = elfInventory `sepBy` newline
  where
    elfInventory = some (decimal <* newline)

example :: String
example =
  unlines
    [ "1000",
      "2000",
      "3000",
      "",
      "4000",
      "",
      "5000",
      "6000",
      "",
      "7000",
      "8000",
      "9000",
      "",
      "10000"
    ]
