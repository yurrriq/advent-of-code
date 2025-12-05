{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day05 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (count)
import Data.Ix (inRange)
import Relude
import Text.Trifecta (Parser, char, decimal, newline, sepEndBy)

database :: Parser ([(Integer, Integer)], [Integer])
database = do
  idRange <- some $ do
    from <- decimal <* char '-'
    to <- decimal <* newline
    pure (from, to)
  ingredients <- newline *> decimal `sepEndBy` newline
  pure (idRange, ingredients)

getExample :: IO ([(Integer, Integer)], [Integer])
getExample = parseString database example

example :: String
example =
  "3-5\n\
  \10-14\n\
  \16-20\n\
  \12-18\n\
  \\n\
  \1\n\
  \5\n\
  \8\n\
  \11\n\
  \17\n\
  \32"

getInput :: IO ([(Integer, Integer)], [Integer])
getInput = parseInputAoC 2025 5 database

partOne :: SimplePuzzle ([(Integer, Integer)], [Integer]) Int
partOne =
  asks \(ranges, ingredients) ->
    count (flip any ranges . flip inRange) ingredients

partTwo :: SimplePuzzle ([(Integer, Integer)], [Integer]) ()
partTwo = fail "not yet implemented"

main :: IO ()
main = $(defaultMainPuzzle)
