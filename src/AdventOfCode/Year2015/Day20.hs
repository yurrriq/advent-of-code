module AdventOfCode.Year2015.Day20 where

import AdventOfCode.TH (defaultMain)
import qualified Data.Set as S
import Math.NumberTheory.ArithmeticFunctions

main :: IO ()
main = $(defaultMain)

getInput :: IO Int
getInput = pure 33100000

partOne :: Int -> Int
partOne = findHouse presents

partTwo :: Int -> Int
partTwo = findHouse presents'

findHouse :: (Int -> Int) -> Int -> Int
findHouse f n = head $ dropWhile ((< n) . f) [1 ..]

-- https://oeis.org/A326122
presents :: Int -> Int
presents = (10 *) . sigma 1

presents' :: Int -> Int
presents' i = 11 * sum (S.filter ((i <=) . (50 *)) (divisors i))
