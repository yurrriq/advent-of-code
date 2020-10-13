module Data.AOC19.Day04 where

import Control.Monad (liftM2)
import Data.Digits (digits)
import Data.List (group)
import Data.List.Ordered (isSorted)

input :: [[Int]]
input = digits 10 <$> [236491 .. 713787]

partOne :: Int
partOne = length $ filter isPossiblePassword input
  where
    isPossiblePassword :: [Int] -> Bool
    isPossiblePassword = liftM2 (&&) isSorted hasDouble
    hasDouble :: Eq a => [a] -> Bool
    hasDouble = any ((>= 2) . length) . group

partTwo :: Int
partTwo = length $ filter isPossiblePassword input
  where
    isPossiblePassword :: [Int] -> Bool
    isPossiblePassword = liftM2 (&&) isSorted hasDouble
    hasDouble :: Eq a => [a] -> Bool
    hasDouble = any ((== 2) . length) . group
