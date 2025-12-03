{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day03 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (maybeFail, (<.>))
import Data.Char (digitToInt)
import Data.Foldable (maximum)
import Relude
import Text.Trifecta (digit, newline, sepEndBy)

getExample :: IO [[Int]]
getExample = parseString (some (digitToInt <$> digit) `sepEndBy` newline) example

example :: String
example =
  "987654321111111\n\
  \811111111111119\n\
  \234234234234278\n\
  \818181911112111"

largestPair :: [Int] -> Maybe Int
largestPair bank =
  viaNonEmpty tail (scanr1 max bank) <&> \oneses ->
    maximum (zipWith (\tens ones -> 10 * tens + ones) bank oneses)

partOne :: SimplePuzzle [[Int]] Int
partOne = ask >>= maybeFail "ope!" . sum <.> traverse largestPair

getInput :: IO [[Int]]
getInput =
  parseInputAoC 2025 3
    $ some (digitToInt <$> digit)
    `sepEndBy` newline

partTwo :: SimplePuzzle [[Int]] ()
partTwo = fail "not yet implemented"

main :: IO ()
main = $(defaultMainPuzzle)
