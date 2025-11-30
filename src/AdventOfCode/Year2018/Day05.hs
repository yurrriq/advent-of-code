{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2018.Day05 where

import AdventOfCode.Input (rawInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.Algebra.Free (foldMapFree, returnFree)
import Data.Char (isLower, isUpper, toLower)
import Data.Finite (Finite, finite, finites)
import Data.Group (invert)
import Data.Group.Free (FreeGroupL)
import Data.Group.Free qualified as FG
import Data.List (minimum)
import Relude

-- | Units' types are represented by letters, modelled by a finite number type
-- inhabited by exactly 26 values.
type Unit = Finite 26

-- | A polymer is represented by \(F(26)\).
type Polymer = FreeGroupL Unit

main :: IO ()
main = $(defaultMainPuzzle)

-- | Solve Part One.
partOne :: SimplePuzzle String Int
partOne = asks (order . foldMap inject)

-- | Solve Part Two.
partTwo :: SimplePuzzle String Int
partTwo =
  asks $ foldMap inject >>> \polymer ->
    minimum (map (order . flip clean polymer) finites)

getInput :: IO String
getInput = rawInputAoC 2018 5

-- $setup
--
-- >>> let example = foldMap inject "dabAcCaCBAcCcaDA"

-- | Inject a given character into \(F(26)\).
--
-- N.B. Nonalphabetic characters map to the group identity.
inject :: Char -> Polymer
inject c
  | isLower c = returnFree unit
  | isUpper c = invert (returnFree unit)
  | otherwise = mempty
  where
    unit = finite . toInteger $ ((-) `on` ord) (toLower c) 'a'

-- | Fully react a polymer after removing all instances of a given unit.
--
-- >>> order $ clean 0 example
-- 6
-- >>> order $ clean 1 example
-- 8
-- >>> order $ clean 2 example
-- 4
-- >>> order $ clean 3 example
-- 6
clean :: Unit -> Polymer -> Polymer
clean badUnit = foldMapFree $ \unit ->
  if unit == badUnit
    then mempty
    else returnFree unit

-- | Compute the order of a 'FreeGroupL'.
--
-- >>> order example
-- 10
order :: FreeGroupL a -> Int
order = length . FG.toList
