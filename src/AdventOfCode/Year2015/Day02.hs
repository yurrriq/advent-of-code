{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2015.Day02
  ( main,
    partOne,
    partTwo,
    getInput,
  )
where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Control.Foldl qualified as Foldl
import Control.Lens (view)
import Control.Monad (ap)
import Linear (V3 (..), _yzx)
import Relude
import Text.Trifecta (char, natural)

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle [V3 Integer] (Maybe Integer)
partOne = asks (fmap getSum . foldMap paperNeededFor)

partTwo :: SimplePuzzle [V3 Integer] (Maybe Integer)
partTwo = asks (fmap getSum . foldMap ribbonNeededFor)

getInput :: IO [V3 Integer]
getInput = parseInputAoC 2015 2 (some box)
  where
    box =
      V3
        <$> (natural <* char 'x')
        <*> (natural <* char 'x')
        <*> natural

paperNeededFor :: (Ord a, Num a) => V3 a -> Maybe (Sum a)
paperNeededFor =
  (*) `ap` view _yzx >>> Foldl.fold do
    m <- Foldl.minimum
    z <- 2 * Foldl.sum
    return (Sum . (z +) <$> m)

ribbonNeededFor :: (Ord a, Num a) => V3 a -> Maybe (Sum a)
ribbonNeededFor = Foldl.fold do
  p <- Foldl.product
  z <- Foldl.sum
  x <- Foldl.maximum
  return (Sum . (p +) . (2 *) . (z -) <$> x)
