module AdventOfCode.Year2021.Day03 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.Functor (($>))
import Data.List (foldl1')
import Text.Trifecta (char, newline, sepEndBy, some)

main :: IO ()
main = $(defaultMain)

getInput :: IO [[Bool]]
getInput = parseInput (some bit `sepEndBy` newline) $(inputFilePath)
  where
    bit =
      char '0' $> False
        <|> char '1' $> True

example :: [[Bool]]
example =
  map
    (map (toEnum . digitToInt))
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

partOne :: [[Bool]] -> Int
partOne diagnostics = gammaRate * epsilonRate
  where
    gammaRate = mkRate (>)
    epsilonRate = mkRate (<)

    mkRate cmp = binToDec ((`cmp` pivot) <$> sums)
    pivot = mkPivot diagnostics
    sums = mkSums diagnostics

partTwo :: [[Bool]] -> Int
partTwo = (generatorRating 0 &&& scrubberRating 0) >>> uncurry (*)
  where
    generatorRating = keepBit (>=)
    scrubberRating = keepBit (<)

    keepBit cmp n diagnostics =
      case filter p diagnostics of
        [rating] -> binToDec rating
        diagnostics' -> keepBit cmp (succ n) diagnostics'
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
