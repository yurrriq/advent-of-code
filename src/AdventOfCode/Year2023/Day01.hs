module AdventOfCode.Year2023.Day01 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Text.Trifecta hiding (parseString)

main :: IO ()
main = $(defaultMain)

partOne :: [Int] -> Int
partOne = sum

partTwo :: [Int] -> Int
partTwo = undefined

getInput :: IO [Int]
getInput = parseInput calibrationDocument $(inputFilePath)

calibrationDocument :: Parser [Int]
calibrationDocument = calibrationValue `sepEndBy` newline

calibrationValue :: Parser Int
calibrationValue =
  some digit' <&> \ds ->
    10 * digitToInt (head ds) + digitToInt (last ds)
  where
    digit' = (many letter *> digit) <* many letter

example :: String
example =
  unlines
    [ "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    ]
