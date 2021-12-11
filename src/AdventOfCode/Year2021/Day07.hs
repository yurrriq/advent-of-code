module AdventOfCode.Year2021.Day07 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Text.Trifecta (commaSep, natural)

main :: IO ()
main = $(defaultMain)

getInput :: IO [Int]
getInput = parseInput (map fromInteger <$> commaSep natural) $(inputFilePath)

example :: [Int]
example = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

partOne :: [Int] -> Int
partOne xs = minimum $ IS.foldr outer IM.empty xset
  where
    xset = IS.fromList xs
    outer x m = foldr (inner x) m xs
    inner x y = IM.insertWith (+) x (abs (x - y))

partTwo :: [Int] -> Int
partTwo = undefined
