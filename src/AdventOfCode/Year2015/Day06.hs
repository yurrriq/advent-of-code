-- Thanks to https://github.com/amalloy/advent-of-code/blob/30375f09/day6/src.hs
-- for some inspiration.

module AdventOfCode.Year2015.Day06 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Year2015.Day06.Parsers
import AdventOfCode.Year2015.Day06.Types
import Control.Monad (forM_, mapM_)
import Control.Monad.Trans.State
import Data.Ix (range)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Trifecta (some)

main :: IO ()
main =
  do
    instructions <- parseInput (some instruction) $(inputFilePath)
    putStr "Part One: "
    print $ partOne instructions
    putStr "Part Two: "
    print $ partTwo instructions

partOne :: [Instruction] -> Int
partOne = howBright (Map.empty :: Map Location SimpleLight)

partTwo :: [Instruction] -> Int
partTwo = howBright (Map.empty :: Map Location Dimmer)

howBright :: Light a => Map Location a -> [Instruction] -> Int
howBright emptyGrid instructions =
  foldr ((+) . brightness) 0 $
    execState (mapM_ runInstruction instructions) emptyGrid

runInstruction :: Light a => Instruction -> State (Map Location a) ()
runInstruction (Instruction op locations) = forM_ (range locations) $ \loc ->
  modify (Map.alter (Just . alter op . fromMaybe off) loc)
