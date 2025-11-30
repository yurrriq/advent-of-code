{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2021.Day03 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.List (foldl1', (!!))
import Relude
import Text.Trifecta (char, newline, sepEndBy)

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO [[Bool]]
getInput = parseInputAoC 2021 3 (some bit `sepEndBy` newline)
  where
    bit =
      char '0'
        $> False
        <|> char '1'
        $> True

example :: [[Bool]]
example =
  map
    (map (== '1'))
    [ "00100",
      "11110",
      "10110",
      "10111",
      "10101",
      "01111",
      "00111",
      "11100",
      "10000",
      "11001",
      "00010",
      "01010"
    ]

partOne :: SimplePuzzle [[Bool]] Int
partOne =
  asks
    $ (mkPivot &&& mkSums)
    >>> (mkRate (>) &&& mkRate (<))
    >>> uncurry (*)
  where
    mkRate cmp (pivot, sums) = binToDec ((`cmp` pivot) <$> sums)

partTwo :: SimplePuzzle [[Bool]] Int
partTwo = asks $ (generatorRating 0 &&& scrubberRating 0) >>> uncurry (*)
  where
    generatorRating = keepBit (>=)
    scrubberRating = keepBit (<)

    keepBit cmp n diagnostics =
      case filter p diagnostics of
        [rating] -> binToDec rating
        diagnostics' -> keepBit cmp (n + 1) diagnostics'
      where
        p = bool not id (sums !! n `cmp` pivot) . (!! n)
        pivot = mkPivot diagnostics
        sums = mkSums diagnostics

binToDec :: [Bool] -> Int
binToDec = foldl' (\z b -> fromEnum b + 2 * z) 0

mkPivot :: [a] -> Double
mkPivot = (/ 2) . fromIntegral . length

mkSums :: [[Bool]] -> [Double]
mkSums =
  map fromIntegral
    . foldl1' (zipWith (+))
    . map (map fromEnum)
