{-# LANGUAGE DerivingVia #-}

module AdventOfCode.Year2018.Day01 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMainMaybe, inputFilePath)
import AdventOfCode.Util (findFirstDup, scan)
import Data.Monoid (Sum (..))
import Text.Trifecta (integer, some)

newtype FrequencyChange = FrequencyChange
  {unFrequencyChange :: Integer}
  deriving stock
    (Eq, Ord, Show)
  deriving
    (Semigroup, Monoid)
    via (Sum Integer)

main :: IO ()
main = $(defaultMainMaybe)

getInput :: IO [FrequencyChange]
getInput = parseInput (some (FrequencyChange <$> integer)) $(inputFilePath)

partOne :: [FrequencyChange] -> Maybe Integer
partOne = Just . unFrequencyChange . mconcat

partTwo :: [FrequencyChange] -> Maybe Integer
partTwo =
  fmap unFrequencyChange
    . findFirstDup
    . scan
    . cycle
