module AdventOfCode.Year2021.Day03 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Data.Foldable (foldl')
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
    gammaRate =
      binToDec $
        map (> 500) sums
    epsilonRate =
      binToDec $
        map (< 500) sums
    sums =
      foldl1' (zipWith (+)) $
        map (map fromEnum) diagnostics

partTwo :: [[Bool]] -> Int
partTwo = undefined

binToDec :: [Bool] -> Int
binToDec = foldl' (\z b -> fromEnum b + 2 * z) 0
