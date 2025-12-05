{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day05 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (count)
import Data.Interval (Extended (..), Interval, (<=..<=))
import Data.Interval qualified as Interval
import Data.IntervalSet qualified as ISet
import Data.List.Extra (sumOn')
import Relude
import Text.Trifecta (Parser, char, decimal, newline, sepEndBy)

database :: Parser ([Interval Integer], [Integer])
database = do
  idRange <- some $ do
    from <- decimal <* char '-'
    to <- decimal <* newline
    pure (Finite from <=..<= Finite to)
  ingredients <- newline *> decimal `sepEndBy` newline
  pure (idRange, ingredients)

getExample :: IO ([Interval Integer], [Integer])
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

getInput :: IO ([Interval Integer], [Integer])
getInput = parseInputAoC 2025 5 database

partOne :: SimplePuzzle ([Interval Integer], [Integer]) Int
partOne =
  asks \(ranges, ingredients) ->
    count (flip any ranges . Interval.member) ingredients

partTwo :: SimplePuzzle ([Interval Integer], [Integer]) Integer
partTwo =
  sumOn' ((1 +) . Interval.width)
    <$> withPuzzle fst (asks (ISet.toList . ISet.fromList))

main :: IO ()
main = $(defaultMainPuzzle)
