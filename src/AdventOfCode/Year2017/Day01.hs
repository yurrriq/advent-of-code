module AdventOfCode.Year2017.Day01
  ( main,
    getInput,
    digits,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Data.Char (digitToInt)
import Data.Monoid (Sum (..))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Text.Trifecta (Parser, digit, some)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: " *> print (partOne input)
    putStr "Part Two: " *> print (partTwo input)

getInput :: IO (Vector (Sum Int))
getInput = parseInput digits $(inputFilePath)

digits :: Parser (Vector (Sum Int))
digits = V.fromList <$> some (Sum . digitToInt <$> digit)

partOne :: Vector (Sum Int) -> Int
partOne = day01 1

partTwo :: Vector (Sum Int) -> Int
partTwo = day01 =<< (`div` 2) . V.length

day01 :: (Eq a, Num a) => Int -> Vector (Sum a) -> a
day01 k v = getSum $ V.ifoldl' go mempty v
  where
    go z i x
      | x == v V.! ((i + k) `mod` n) = z <> x
      | otherwise = z
    n = V.length v
