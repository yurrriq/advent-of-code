module AdventOfCode.Year2015.Day10
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.TH (defaultMain)
import Data.Char (digitToInt)
import Data.List.Infinite qualified as Infinite
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

main :: IO ()
main = $(defaultMain)

getInput :: IO (NonEmpty Int)
getInput = pure (NE.fromList (map digitToInt "1113222113"))

partOne :: NonEmpty Int -> Int
partOne ds = NE.length (Infinite.iterate expand ds Infinite.!! 40)

partTwo :: NonEmpty Int -> Int
partTwo ds = NE.length (Infinite.iterate expand ds Infinite.!! 50)

expand :: NonEmpty Int -> NonEmpty Int
expand = NE.fromList . concatMap go . NE.group
  where
    go ds = [NE.length ds, NE.head ds]
