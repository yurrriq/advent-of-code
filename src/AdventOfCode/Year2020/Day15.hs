module AdventOfCode.Year2020.Day15
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import Control.Monad.State (State, evalState, execState, get, put)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print (partOne input)
    putStr "Part Two: "
    print (partTwo input)

getInput :: IO [Int]
getInput = pure [0, 14, 1, 3, 7, 9]

partOne :: [Int] -> Int
partOne = memoryGame 2020

partTwo :: [Int] -> Int
partTwo = memoryGame 30000000

memoryGame :: Int -> [Int] -> Int
memoryGame n input =
  evalState memoryRound $ (!! pred n) $
    iterate (execState memoryRound) (input, 1, IM.empty)

memoryRound :: State ([Int], Int, IntMap Int) Int
memoryRound =
  do
    (input, now, seen) <- get
    case input of
      [] -> put ([0], now, seen) *> memoryRound
      current : rest ->
        case IM.lookup current seen of
          Nothing ->
            do
              put (rest, now + 1, IM.insert current now seen)
              pure current
          Just before ->
            do
              put (now - before : rest, now + 1, IM.insert current now seen)
              pure current
