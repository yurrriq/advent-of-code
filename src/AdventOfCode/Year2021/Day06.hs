{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2021.Day06 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Control.Lens (over)
import Data.Finite (Finite)
import Data.Matrix (Matrix (..), colVector, matrix)
import Data.Vector.Sized qualified as VS
import Foreign.Marshal.Utils (fromBool)
import Relude
import Text.Trifecta (commaSep, natural)

type Lanternfish = Finite 9

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO (Matrix Int)
getInput = mkColumn <$> parseInputAoC 2021 6 (commaSep (fromInteger <$> natural))

getExample :: IO (Matrix Int)
getExample = pure (mkColumn example)

example :: [Lanternfish]
example = [3, 4, 3, 1, 2]

partOne :: SimplePuzzle (Matrix Int) Int
partOne = simulate 80

partTwo :: SimplePuzzle (Matrix Int) Int
partTwo = simulate 256

simulate :: Int -> SimplePuzzle (Matrix Int) Int
simulate n = asks (sum . ((day ^ n) *))

mkColumn :: (Foldable t) => t Lanternfish -> Matrix Int
mkColumn =
  colVector
    . VS.fromSized
    . foldr ((`over` (+ 1)) . VS.ix) (pure 0)

simulateDay :: Matrix Int -> Matrix Int
simulateDay timers = day * timers

day :: Matrix Int
day = matrix 9 9 $ \case
  (6, 7) -> 1
  (7, 1) -> 1
  (9, 1) -> 1
  (y, x) -> fromBool (x == y + 1)
