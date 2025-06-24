module AdventOfCode.Year2020.Day10 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (frequenciesInt, snoc)
import Data.IntMap ((!))
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List (sort)
import Text.Trifecta (natural, some)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne input
    putStr "Part Two: "
    print $ partTwo input

getInput :: IO [Int]
getInput = sort <$> parseInput (some (fromInteger <$> natural)) $(inputFilePath)

partOne :: [Int] -> Int
partOne adapters = (connections ! 1) * (connections ! 3)
  where
    connections = frequenciesInt (zipWith (-) (tail chain) chain)
    chain = 0 : snoc adapters builtIn
    builtIn = maximum adapters + 3

partTwo :: [Int] -> Int
partTwo adapters = paths ! 0
  where
    paths = IM.fromSet countPaths adaptersSet
    countPaths from
      | from == builtIn = 1
      | otherwise = sum [IM.findWithDefault 0 (from + k) paths | k <- [1 .. 3]]
    adaptersSet = IS.fromList (0 : adapters ++ [builtIn])
    builtIn = maximum adapters + 3
