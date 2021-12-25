module AdventOfCode.Year2021.Day09 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Lens (ifoldl')
import Data.Char (digitToInt)
import Data.Ix (Ix, inRange)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))
import Text.Trifecta

main :: IO ()
main = $(defaultMain)

getInput :: IO (Map (V2 Int) Int)
getInput = parseInput model $(inputFilePath)

example :: Map (V2 Int) Int
example =
  mkModel
    [ [2, 1, 9, 9, 9, 4, 3, 2, 1, 0],
      [3, 9, 8, 7, 8, 9, 4, 9, 2, 1],
      [9, 8, 5, 6, 7, 8, 9, 8, 9, 2],
      [8, 7, 6, 7, 8, 9, 6, 7, 8, 9],
      [9, 8, 9, 9, 9, 6, 5, 6, 7, 8]
    ]

partOne :: Map (V2 Int) Int -> Int
partOne m = M.foldl' ((+) . succ) 0 (findLowPoints m)

partTwo :: Map (V2 Int) Int -> Int
partTwo = undefined

model :: Parser (Map (V2 Int) Int)
model = mkModel <$> some (digitToInt <$> digit) `sepEndBy` newline

mkModel :: [[Int]] -> Map (V2 Int) Int
mkModel = ifoldl' (ifoldl' . go) M.empty
  where
    go y x acc n = M.insert (V2 x y) n acc

findLowPoints :: Map (V2 Int) Int -> Map (V2 Int) Int
findLowPoints m = M.filterWithKey (isLowPoint m bounds) m
  where
    bounds = (fst (M.findMin m), fst (M.findMax m))

isLowPoint :: Map (V2 Int) Int -> (V2 Int, V2 Int) -> V2 Int -> Int -> Bool
isLowPoint _ _ _ 9 = False
isLowPoint m bounds point n = all ((n <) . (m M.!)) (neighborsInRange bounds point)

-- FIXME: generalize
neighborsInRange :: (Ix a, Num a) => (V2 a, V2 a) -> V2 a -> Set (V2 a)
neighborsInRange range point = S.filter (inRange range) (neighborsOf' point)
  where
    neighborsOf' = S.fromList . flip map adjacencies' . (+)
    adjacencies' = [V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1]
