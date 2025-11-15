{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2018.Day01 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.SimplePuzzle
import AdventOfCode.TH (evalPuzzle)
import AdventOfCode.Util (findFirstDup, maybeFail, scan)
import Relude
import Text.Trifecta (integer)

newtype FrequencyChange = FrequencyChange
  {unFrequencyChange :: Integer}
  deriving stock
    (Eq, Ord, Show)
  deriving
    (Semigroup, Monoid)
    via (Sum Integer)

main :: IO ()
main = $(evalPuzzle)

getInput :: IO [FrequencyChange]
getInput = parseInputAoC 2018 1 (some (FrequencyChange <$> integer))

partOne :: SimplePuzzle [FrequencyChange] Integer
partOne = asks $ unFrequencyChange . mconcat

partTwo :: SimplePuzzle [FrequencyChange] Integer
partTwo =
  ask
    >>= maybeFail "ope!"
    . fmap unFrequencyChange
    . findFirstDup
    . scan
    . cycle
