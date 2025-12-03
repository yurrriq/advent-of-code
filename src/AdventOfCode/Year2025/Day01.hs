{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day01 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (count)
import Relude
import Text.Trifecta (Parser, char, decimal, newline, sepEndBy)

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle [Integer] Int
partOne = asks (count (== 0) . scanl' rotate 50)
  where
    rotate delta position = (position + delta) `mod` 100

partTwo :: SimplePuzzle [Integer] Int
partTwo = fail "Not yet implemented"

getInput :: IO [Integer]
getInput = parseInputAoC 2025 1 (rotation `sepEndBy` newline)

getExample :: IO [Integer]
getExample = parseString (rotation `sepEndBy` newline) example

example :: String
example =
  "L68\n\
  \L30\n\
  \R48\n\
  \L5\n\
  \R60\n\
  \L55\n\
  \L1\n\
  \L99\n\
  \R14\n\
  \L82"

rotation :: Parser Integer
rotation = (negate <$ char 'L' <*> decimal) <|> (char 'R' *> decimal)
