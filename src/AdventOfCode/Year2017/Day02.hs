{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2017.Day02 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.SimplePuzzle
import AdventOfCode.TH (evalPuzzle)
import AdventOfCode.Util (holes)
import Data.List.Extra (maximum, minimum, sumOn')
import Relude
import Text.Trifecta (decimal, newline, sepBy1, sepEndBy1, tab)

main :: IO ()
main = $(evalPuzzle)

getInput :: IO [[Int]]
getInput = parseInputAoC 2017 2 (row `sepEndBy1` newline)
  where
    row = int `sepBy1` tab
    int = fromInteger <$> decimal

partOne :: SimplePuzzle [[Int]] Int
partOne = asks (sumOn' (uncurry (-) . (maximum &&& minimum)))

partTwo :: SimplePuzzle [[Int]] Int
partTwo =
  asks $ sum . mapMaybe \row ->
    listToMaybe do
      (x, ys) <- holes row
      y <- ys
      if
        | (q, 0) <- quotRem x y -> pure q
        | (q, 0) <- quotRem y x -> pure q
        | otherwise -> mzero
