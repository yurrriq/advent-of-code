{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2020.Day17
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (neighborsOf)
import Control.Lens (ifoldl', set)
import Data.List.Infinite ((!!))
import Data.List.Infinite qualified as Infinite
import Data.Map qualified as Map
import Data.Set qualified as Set
import Linear (R2 (..), V2 (..), V3 (..), V4 (..))
import Relude
import Text.Trifecta

type Domain f =
  ( Applicative f,
    Traversable f,
    R2 f,
    Num (f Int),
    Ord (f Int)
  ) ::
    Constraint

type Codomain f a = Set (f Int) -> a

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO [[Bool]]
getInput = parseInputAoC 2020 17 cubeRegion

partOne :: SimplePuzzle [[Bool]] Int
partOne = asks (solve . mkPocketDimension @V3)

partTwo :: SimplePuzzle [[Bool]] Int
partTwo = asks (solve . mkPocketDimension @V4)

solve :: (Domain f) => Codomain f Int
solve = Set.size . (!! 6) . Infinite.iterate stepCycle

stepCycle :: (Domain f) => Codomain f (Set (f Int))
stepCycle activeCubes = stillActive <> activated
  where
    stillActive = filterNeighborCounts (\n -> n == 2 || n == 3) activeNeighborCounts
    activated = filterNeighborCounts (3 ==) inactiveNeighborCounts
    activeNeighborCounts = neighborCounts `Map.restrictKeys` activeCubes
    inactiveNeighborCounts = neighborCounts `Map.withoutKeys` activeCubes
    filterNeighborCounts p = Map.keysSet . Map.filter p
    neighborCounts =
      Map.unionsWith ((+) :: Int -> Int -> Int)
        $ Map.fromSet (const 1)
        . neighborsOf
        <$> Set.toList activeCubes

mkPocketDimension :: (Domain f) => [[Bool]] -> Set (f Int)
mkPocketDimension = ifoldl' (ifoldl' . go) Set.empty
  where
    go y x activeCubes True = Set.insert (set _xy (V2 x y) 0) activeCubes
    go _ _ activeCubes False = activeCubes

cubeRegion :: Parser [[Bool]]
cubeRegion = some cube `sepEndBy` newline
  where
    cube =
      char '#'
        $> True
        <|> char '.'
        $> False
