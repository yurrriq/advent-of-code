{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day02 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (numDigits)
import Data.FastDigits (digits)
import Data.Ix (range)
import Data.List.Extra (sumOn')
import Relude
import Text.Trifecta (Parser, char, commaSep, natural)

idRange :: Parser (Integer, Integer)
idRange = (,) <$> natural <* char '-' <*> natural

getExample :: IO [(Integer, Integer)]
getExample = parseString (commaSep idRange) example

example :: String
example =
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,\
  \1698522-1698528,446443-446449,38593856-38593862,565653-565659,\
  \824824821-824824827,2121212118-2121212124"

getInput :: IO [(Integer, Integer)]
getInput = parseInputAoC 2025 2 (commaSep idRange)

partOne :: SimplePuzzle [(Integer, Integer)] Integer
partOne = asks (sumOn' (sum . filter isA020338 . range))

-- https://oeis.org/A020338
isA020338 :: Integer -> Bool
isA020338 n = {- n > 0 && -} even k && low == high
  where
    k = numDigits n
    (low, high) = splitAt (k `div` 2) (digits 10 n)

partTwo :: SimplePuzzle [(Integer, Integer)] Integer
partTwo = fail "not yet implemented"

main :: IO ()
main = $(defaultMainPuzzle)
