module AdventOfCode.Year2015.Day25 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Monad (void)
import Text.Trifecta (Parser, comma, natural, symbol)

type Coordinates = (Integer, Integer)

main :: IO ()
main =
  do
    putStr "Part One: "
    print . partOne =<< getInput

partOne :: Coordinates -> Integer
partOne (column, row) = 20151125 * (252533 ^ (n - 1)) `mod` 33554393
  where
    n = a000124 row + a000217 (column + row - 1) - a000217 row

getInput :: IO Coordinates
getInput = parseInput coordinates $(inputFilePath)

coordinates :: Parser Coordinates
coordinates =
  do
    void $ symbol "To continue, please consult the code grid in the manual."
    row <- symbol "Enter the code at row" *> natural <* comma
    column <- symbol "column" *> natural
    pure (column, row)

-- https://oeis.org/A000124
-- https://en.wikipedia.org/wiki/Lazy_caterer's_sequence
a000124 :: (Integral a) => a -> a
a000124 n = n * (n - 1) `div` 2 + 1
{-# SPECIALIZE INLINE a000124 :: Integer -> Integer #-}

-- https://oeis.org/A000217
-- https://en.wikipedia.org/wiki/Triangular_number
a000217 :: (Integral a) => a -> a
a000217 n = n * (n + 1) `div` 2
{-# SPECIALIZE INLINE a000217 :: Integer -> Integer #-}
