{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day03 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (maybeFail)
import Data.Char (digitToInt)
import Data.Vector qualified as Vector
import Relude
import Text.Trifecta (Parser, digit, newline, sepEndBy)

getExample :: IO [[Int]]
getExample = parseString joltageRatings example

joltageRatings :: Parser [[Int]]
joltageRatings = some (digitToInt <$> digit) `sepEndBy` newline

example :: String
example =
  "987654321111111\n\
  \811111111111119\n\
  \234234234234278\n\
  \818181911112111"

largestPossibleJoltage :: Int -> [Int] -> Maybe Int
largestPossibleJoltage 0 _bank = Nothing
largestPossibleJoltage n bank = go n 0 (Vector.fromList bank)
  where
    go 0 number _ = Just number
    go k number ds
      | Vector.null ds = Nothing -- length ds + 1 <= k = Nothing
      | otherwise = do
          let d = Vector.maximum (Vector.take (Vector.length ds - k + 1) ds)
          i <- Vector.elemIndex d ds
          go (k - 1) (number * 10 + d) (Vector.drop (i + 1) ds)

partOne :: SimplePuzzle [[Int]] Int
partOne =
  maybeFail "some bank had fewer than two batteries"
    . fmap sum
    . traverse (largestPossibleJoltage 2)
    =<< ask

getInput :: IO [[Int]]
getInput = parseInputAoC 2025 3 joltageRatings

partTwo :: SimplePuzzle [[Int]] Int
partTwo =
  maybeFail "some bank had fewer than twelve batteries"
    . fmap sum
    . traverse (largestPossibleJoltage 12)
    =<< ask

main :: IO ()
main = $(defaultMainPuzzle)
