module AdventOfCode.Year2019.Day04 where

import AdventOfCode.TH (defaultMain)
import AdventOfCode.Util (count, (<&&>))
import Data.FastDigits (digits)
import Data.List (group)
import Data.List.Ordered (isSorted)

main :: IO ()
main = $(defaultMain)

getInput :: IO [[Int]]
getInput = pure $ reverse . digits 10 <$> [236491 .. 713787]

partOne :: [[Int]] -> Int
partOne = solve (>=)

partTwo :: [[Int]] -> Int
partTwo = solve (==)

solve :: (Int -> Int -> Bool) -> [[Int]] -> Int
solve = count . (isSorted <&&>) . hasDouble
  where
    hasDouble cmp = any ((`cmp` 2) . length) . group
