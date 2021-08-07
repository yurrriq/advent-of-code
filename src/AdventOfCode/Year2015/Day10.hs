module AdventOfCode.Year2015.Day10
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.TH
import Data.Char (digitToInt)
import Data.List (group)

main :: IO ()
main = $(defaultMain)

getInput :: IO [Int]
getInput = pure (map digitToInt "1113222113")

partOne :: [Int] -> Int
partOne ds = length (iterate expand ds !! 40)

partTwo :: [Int] -> Int
partTwo ds = length (iterate expand ds !! 50)

expand :: [Int] -> [Int]
expand = concatMap go . group
  where
    go ds = [length ds, head ds]
