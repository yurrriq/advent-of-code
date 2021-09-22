{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2017.Day05 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Lens (ifoldl')
import Control.Monad.State (State, evalState, get, put)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Text.Trifecta (integer, some)

main :: IO ()
main = $(defaultMain)

getInput :: IO (IntMap Int)
getInput = parseInput jumpOffsets $(inputFilePath)
  where
    jumpOffsets = ifoldl' go IM.empty <$> some (fromInteger <$> integer)
    go i m n = IM.insert i n m

partOne :: IntMap Int -> Int
partOne = evalState (step (+ 1)) . (0,0,)

partTwo :: IntMap Int -> Int
partTwo = evalState (step updateOffset) . (0,0,)
  where
    updateOffset offset
      | offset >= 3 = offset - 1
      | otherwise = offset + 1

step :: (Int -> Int) -> State (Int, Int, IntMap Int) Int
step updateOffset =
  do
    (steps, pos, offsets) <- get
    let offset = offsets IM.! pos
    let nextPos = pos + offset
    if nextPos >= IM.size offsets
      then pure (steps + 1)
      else do
        put (steps + 1, pos + offset, IM.update (Just . updateOffset) pos offsets)
        step updateOffset
