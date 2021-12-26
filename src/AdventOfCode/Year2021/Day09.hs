module AdventOfCode.Year2021.Day09 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Lens (ifoldl')
import Data.Char (digitToInt)
import Data.Ix (Ix, inRange)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down (Down))
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Linear (V2 (..))
import Text.Trifecta (Parser, digit, newline, sepEndBy, some)

type Model = (HeightMap, (Point, Point))

type HeightMap = Map (V2 Int) Height

type Point = V2 Int

type Height = Int

main :: IO ()
main = $(defaultMain)

getInput :: IO Model
getInput = parseInput model $(inputFilePath)

example :: Model
example =
  mkModel
    [ [2, 1, 9, 9, 9, 4, 3, 2, 1, 0],
      [3, 9, 8, 7, 8, 9, 4, 9, 2, 1],
      [9, 8, 5, 6, 7, 8, 9, 8, 9, 2],
      [8, 7, 6, 7, 8, 9, 6, 7, 8, 9],
      [9, 8, 9, 9, 9, 6, 5, 6, 7, 8]
    ]

partOne :: Model -> Int
partOne = sum . fmap succ . findLowPoints

partTwo :: Model -> Int
partTwo modell =
  product . take 3 . sortOn Down $
    map (length . findBasin modell) $
      M.keys (findLowPoints modell)

model :: Parser Model
model = mkModel <$> some (digitToInt <$> digit) `sepEndBy` newline

mkModel :: [[Height]] -> Model
mkModel heightses = (kart, bounds)
  where
    kart = ifoldl' (ifoldl' . go) M.empty heightses
    go y x kartet height = M.insert (V2 x y) height kartet
    bounds = (fst (M.findMin kart), fst (M.findMax kart))

findLowPoints :: Model -> HeightMap
findLowPoints (kart, bounds) = M.filterWithKey (isLowPoint (kart, bounds)) kart

isLowPoint :: Model -> Point -> Height -> Bool
isLowPoint _ _ 9 = False
isLowPoint (kart, bounds) point height =
  all ((height <) . (kart M.!)) $
    neighborsInRange bounds point

findBasin :: Model -> Point -> Set Point
findBasin (kart, bounds) lowPoint = S.foldl' go (S.singleton lowPoint) (S.singleton lowPoint)
  where
    go seen point =
      let neighbors = S.filter ((< 9) . (kart M.!)) (neighborsInRange bounds point) \\ seen
       in if S.null neighbors
            then seen
            else S.foldl' go (seen `S.union` neighbors) neighbors

-- FIXME: generalize
neighborsInRange :: (Ix a, Num a) => (V2 a, V2 a) -> V2 a -> Set (V2 a)
neighborsInRange range point = S.filter (inRange range) (neighborsOf' point)
  where
    neighborsOf' = S.fromList . flip map adjacencies' . (+)
    adjacencies' = [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]
