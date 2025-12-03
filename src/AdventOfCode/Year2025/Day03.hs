{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day03 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (maybeFail, (<.>))
import Data.Char (digitToInt)
import Data.Foldable (maximum)
import Data.Semigroup (Max (..))
import Relude
import Text.Trifecta (digit, newline, sepEndBy)

getExample :: IO [[Int]]
getExample =
  flip parseString example
    $ some (digitToInt <$> digit)
    `sepEndBy` newline

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

largestNumber :: Int -> [Int] -> Maybe Int
largestNumber k bank
  | k <= 0 = Nothing
  | otherwise = getMax <$> go k bank
  where
    go 1 xs = Just (Max (maximum xs))
    go n xs =
      case uncons xs of
        Nothing -> Nothing
        Just (_, []) -> Nothing
        Just (d, ds) ->
          let candidate = (Max d * 10 ^ (n - 1) +) <$> go (n - 1) ds
              nextCandidate = go n ds
           in candidate <> nextCandidate

partTwo :: SimplePuzzle [[Int]] Int
partTwo = ask >>= maybeFail "ope!" . sum <.> traverse (largestNumber 12)

main :: IO ()
main = $(defaultMainPuzzle)
