{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module AdventOfCode.Year2021.Day11 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (neighborsOf)
import Control.Lens (ifoldl')
import Data.Char (digitToInt)
import Data.Foldable (forM_)
import Data.Ix (Ix, inRange)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Linear (V2 (..))
import Text.Trifecta (Parser, digit, newline, sepEndBy, some)

main :: IO ()
main = $(defaultMain)

getInput :: IO (Map (V2 Int) Int)
getInput = parseInput grid $(inputFilePath)

example :: IO (Map (V2 Int) Int)
example =
  parseString grid . unlines $
    [ "5483143223",
      "2745854711",
      "5264556173",
      "6141336146",
      "6357385478",
      "4167524645",
      "2176841721",
      "6882881134",
      "4846848554",
      "5283751526"
    ]

partOne :: Map (V2 Int) Int -> Int
partOne = fst . (!! 100) . iterate step . (0,)

step :: (Int, Map (V2 Int) Int) -> (Int, Map (V2 Int) Int)
step (n, octopuses) =
  ( \(flashes, octopuses') ->
      (n + S.size flashes, foldl (\m coords -> M.insert coords 0 m) octopuses' flashes)
  )
    $ go (S.empty, fmap succ octopuses)

go :: (Set (V2 Int), Map (V2 Int) Int) -> (Set (V2 Int), Map (V2 Int) Int)
go (flashes, octopuses) =
  if any (> 9) octopuses
    then M.foldlWithKey flash (flashes, octopuses) octopuses
    else (flashes, octopuses)

flash :: (Set (V2 Int), Map (V2 Int) Int) -> V2 Int -> Int -> (Set (V2 Int), Map (V2 Int) Int)
flash (flashes, octopuses) coords octopus
  | octopus > 9 =
      let neighbors = neighborsInRange (pure 0, pure 9) coords
          flashes' = S.insert coords flashes
          octopuses' = M.delete coords octopuses
       in go (flashes', foldl (flip (M.alter (fmap succ))) octopuses' neighbors)
  | otherwise = (flashes, octopuses)

partTwo :: Map (V2 Int) Int -> Int
partTwo = undefined

grid :: Parser (Map (V2 Int) Int)
grid = mkGrid <$> some (digitToInt <$> digit) `sepEndBy` newline

mkGrid :: [[a]] -> Map (V2 Int) a
mkGrid = ifoldl' (ifoldl' . f) M.empty
  where
    f y x = flip (M.insert (V2 x y))

-- FIXME: generalize
neighborsInRange :: (Ix a, Num a) => (V2 a, V2 a) -> V2 a -> Set (V2 a)
neighborsInRange range point = S.filter (inRange range) (neighborsOf point)

printGrid :: Map (V2 Int) Int -> IO ()
printGrid m = forM_ [0 .. 9] $ \y ->
  do
    forM_ [0 .. 9] $ \x ->
      putStr (show (m M.! V2 x y))
    putStrLn ""
