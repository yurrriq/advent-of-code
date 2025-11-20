{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2022.Day01 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.Foldable1 (maximum)
import Data.List.NonEmpty qualified as NE
import Relude
import Text.Trifecta (Parser, decimal, newline)

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO (NonEmpty Integer)
getInput = parseInputAoC 2022 1 inventory

partOne :: SimplePuzzle (NonEmpty Integer) Integer
partOne = asks maximum

partTwo :: SimplePuzzle (NonEmpty Integer) Integer
partTwo = asks (sum . NE.take 3 . NE.sortBy (comparing Down))

inventory :: Parser (NonEmpty Integer)
inventory = elfInventory `sepBy1` newline
  where
    elfInventory = sum <$> NE.some1 (decimal <* newline)

example :: String
example =
  "1000\n\
  \2000\n\
  \3000\n\
  \\n\
  \4000\n\
  \\n\
  \5000\n\
  \6000\n\
  \\n\
  \7000\n\
  \8000\n\
  \9000\n\
  \\n\
  \10000\n"
