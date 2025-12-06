{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day06 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.FastDigits (digits, undigits)
import Relude
import Text.Trifecta (Parser, char, decimal, newline, sepEndBy1, symbol)

worksheet :: Parser ([[Integer]], [[Integer] -> Integer])
worksheet = do
  numbers <- decimal `sepEndBy1` some (char ' ') `sepEndBy1` newline
  operations <- some (sum <$ symbol "+" <|> symbol "*" $> product)
  pure (transpose numbers, operations)

getExample :: IO ([[Integer]], [[Integer] -> Integer])
getExample = parseString worksheet example

example :: String
example =
  "123 328  51 64 \n\
  \ 45 64  387 23 \n\
  \  6 98  215 314\n\
  \*   +   *   +  "

getInput :: IO ([[Integer]], [[Integer] -> Integer])
getInput = parseInputAoC 2025 6 worksheet

partOne :: SimplePuzzle ([[Integer]], [[Integer] -> Integer]) Integer
partOne = asks \(numbers, operations) ->
  sum $ zipWith ($) operations numbers

partTwo :: SimplePuzzle ([[Integer]], [[Integer] -> Integer]) ()
partTwo = fail "not yet implemented"

main :: IO ()
main = $(defaultMainPuzzle)
