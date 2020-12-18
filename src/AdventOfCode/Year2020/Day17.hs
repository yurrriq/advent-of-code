module AdventOfCode.Year2020.Day17
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (neighborsOf)
import Control.Applicative ((<|>))
import Control.Lens (ifoldl', set)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (R2 (..), V2 (..), V3 (..), V4 (..))
import Text.Trifecta

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne (mkPocketDimension input)
    putStr "Part Two: "
    print $ partTwo (mkPocketDimension input)

getInput :: IO [[Bool]]
getInput = parseInput region $(inputFilePath)

partOne :: Set (V3 Int) -> Int
partOne = Set.size . (!! 6) . iterate stepCycle

partTwo :: Set (V4 Int) -> Int
partTwo = Set.size . (!! 6) . iterate stepCycle

stepCycle :: (Applicative f, Traversable f, Num (f Int), Ord (f Int)) => Set (f Int) -> Set (f Int)
stepCycle activeCubes = stillActive <> activated
  where
    stillActive = filterNeighborCounts (\n -> n == 2 || n == 3) activeNeighborCounts
    activated = filterNeighborCounts (3 ==) inactiveNeighborCounts
    activeNeighborCounts = neighborCounts `Map.restrictKeys` activeCubes
    inactiveNeighborCounts = neighborCounts `Map.withoutKeys` activeCubes
    filterNeighborCounts p = Map.keysSet . Map.filter p
    neighborCounts =
      Map.unionsWith ((+) :: Int -> Int -> Int) $
        Map.fromSet (const 1) . neighborsOf
          <$> Set.toList activeCubes

mkPocketDimension :: (Applicative f, R2 f, Ord (f Int)) => [[Bool]] -> Set (f Int)
mkPocketDimension = ifoldl' (ifoldl' . go) Set.empty
  where
    go y x activeCubes True = Set.insert (set _xy (V2 x y) (pure 0)) activeCubes
    go _ _ activeCubes False = activeCubes

region :: Parser [[Bool]]
region = some cube `sepEndBy` newline
  where
    cube =
      char '#' $> True
        <|> char '.' $> False
