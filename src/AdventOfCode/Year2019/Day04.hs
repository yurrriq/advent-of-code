module AdventOfCode.Year2019.Day04 where

import Control.Monad (liftM2)
import Data.FastDigits (digits)
import Data.List (group)
import Data.List.Ordered (isSorted)

main :: IO ()
main =
  do
    let input = reverse . digits 10 <$> [236491 .. 713787]
    putStr "Part One: "
    print $ partOne input
    putStr "Part Two: "
    print $ partTwo input

partOne :: [[Int]] -> Int
partOne = length . filter isPossiblePassword
  where
    isPossiblePassword :: [Int] -> Bool
    isPossiblePassword = liftM2 (&&) isSorted hasDouble
    hasDouble :: Eq a => [a] -> Bool
    hasDouble = any ((>= 2) . length) . group

partTwo :: [[Int]] -> Int
partTwo = length . filter isPossiblePassword
  where
    isPossiblePassword :: [Int] -> Bool
    isPossiblePassword = liftM2 (&&) isSorted hasDouble
    hasDouble :: Eq a => [a] -> Bool
    hasDouble = any ((== 2) . length) . group
