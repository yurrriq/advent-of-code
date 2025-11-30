{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2017.Day03
  ( main,
    getInput,
    partOne,
  )
where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (neighborsOf)
import Data.List.Infinite (Infinite (..), (!!), (...))
import Data.List.Infinite qualified as Infinite
import Data.Map qualified as Map
import Linear (V2 (..))
import Relude
import Text.Trifecta (integer)

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO Word
getInput = parseInputAoC 2017 3 (fromInteger <$> integer)

partOne :: SimplePuzzle Word Int
partOne = asks (sum . abs . (spiral !!) . pred)

-- https://oeis.org/A141481
partTwo :: SimplePuzzle Word Word
partTwo = go (Map.singleton 0 1) (Infinite.tail spiral)
  where
    go seen (here :< there) = do
      let nextValue = sum $ Map.restrictKeys seen (neighborsOf here)
      asks (< nextValue)
        >>= bool (go (Map.insert here nextValue seen) there) (pure nextValue)

spiral :: Infinite (V2 Int)
spiral =
  Infinite.scanl' (+) 0
    $ Infinite.concat
    $ Infinite.zipWith replicateNE (Infinite.concatMap (replicateNE 2) (1 ...))
    $ Infinite.cycle (V2 1 0 :| [V2 0 1, V2 (-1) 0, V2 0 (-1)])

replicateNE :: (HasCallStack) => Int -> a -> NonEmpty a
replicateNE n x
  | n > 0 = x :| replicate (n - 1) x
  | otherwise = error "must be positive"
