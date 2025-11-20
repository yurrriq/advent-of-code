{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2017.Day01
  ( main,
    getInput,
    digits,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.Char (digitToInt)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Relude
import Text.Trifecta (Parser, digit)

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO (Vector (Sum Int))
getInput = parseInputAoC 2017 1 digits

digits :: Parser (Vector (Sum Int))
digits = V.fromList <$> some (Sum . digitToInt <$> digit)

partOne :: SimplePuzzle (Vector (Sum Int)) Int
partOne = asks (day01 1)

partTwo :: SimplePuzzle (Vector (Sum Int)) Int
partTwo = asks (day01 =<< (`div` 2) . V.length)

day01 :: (Eq a, Num a) => Int -> Vector (Sum a) -> a
day01 k v = getSum $ V.ifoldl' go mempty v
  where
    go z i x
      | x == v V.! ((i + k) `mod` n) = z <> x
      | otherwise = z
    n = V.length v
