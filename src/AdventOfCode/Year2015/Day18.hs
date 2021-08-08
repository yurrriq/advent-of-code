module AdventOfCode.Year2015.Day18 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (neighborsOf)
import Control.Applicative ((<|>))
import Control.Lens (ifoldl', set)
import Data.Functor (($>))
import Data.Ix (inRange)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (R2 (..), V2 (..))
import Text.Trifecta (CharParsing (char), Parser, newline, sepEndBy, some)
import Prelude

main :: IO ()
main = $(defaultMain)

getInput :: IO (Set (V2 Int))
getInput = parseInput grid $(inputFilePath)

partOne :: Set (V2 Int) -> Int
partOne = doPartOne 100 100

doPartOne :: Int -> Int -> Set (V2 Int) -> Int
doPartOne size i = Set.size . (!! i) . iterate (stepAnimation bounds)
  where
    bounds = (pure 0, pure (size - 1))

partTwo :: Set (V2 Int) -> Int
partTwo = doPartTwo 100 100

doPartTwo :: Int -> Int -> Set (V2 Int) -> Int
doPartTwo size i =
  Set.size . (!! i)
    . iterate (lightCorners . stepAnimation bounds)
    . lightCorners
  where
    lightCorners = Set.union (Set.fromList (sequenceA (pure [0, size - 1])))
    bounds = (pure 0, pure (size - 1))

stepAnimation :: (V2 Int, V2 Int) -> Set (V2 Int) -> Set (V2 Int)
stepAnimation bounds lightsOn = stillOn <> lit
  where
    stillOn = filterNeighborCounts (`elem` [2, 3]) litNeighborCounts
    lit = filterNeighborCounts (3 ==) dimNeighborCounts
    litNeighborCounts = neighborCounts `Map.restrictKeys` lightsOn
    dimNeighborCounts = neighborCounts `Map.withoutKeys` lightsOn
    filterNeighborCounts p = Map.keysSet . Map.filter p
    neighborCounts =
      Map.unionsWith (+) $
        Map.fromSet (const (1 :: Int)) . Set.filter (inRange bounds) . neighborsOf
          <$> Set.toList lightsOn

grid :: Parser (Set (V2 Int))
grid = mkGrid <$> some light `sepEndBy` newline
  where
    light =
      char '#' $> True
        <|> char '.' $> False

mkGrid :: [[Bool]] -> Set (V2 Int)
mkGrid = ifoldl' (ifoldl' . go) Set.empty
  where
    go y x lightsOn True = Set.insert (set _xy (V2 x y) (pure 0)) lightsOn
    go _ _ lightsOn False = lightsOn
