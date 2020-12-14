{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Year2020.Day14 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Year2020.Day14.Parsers
import AdventOfCode.Year2020.Day14.Types
import Control.Applicative (some)
import Control.Lens
import Control.Lens.Indexed (ifoldl', ifoldlM)
import Control.Monad.State (State, execState, get)
import Data.Bits (clearBit, setBit)
import qualified Data.IntMap as IM

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne input
    putStr "Part Two: "
    print $ partTwo input

getInput :: IO [Instruction]
getInput = parseInput (some instruction) $(inputFilePath)

partOne :: [Instruction] -> Int
partOne = execProgram runInstructionV1

runInstructionV1 :: Instruction -> State ProgState ()
runInstructionV1 (SetMask newMask) = mask .= newMask
runInstructionV1 (SetValue addr val) =
  do
    newVal <- applyMaskV1 val <$> view mask <$> get
    memory %= IM.insert addr newVal

applyMaskV1 :: Int -> [Maybe Bool] -> Int
applyMaskV1 = ifoldl' $ \i x -> \case
  Nothing -> x
  Just False -> x `clearBit` i
  Just True -> x `setBit` i

partTwo :: [Instruction] -> Int
partTwo = execProgram runInstructionV2

runInstructionV2 :: Instruction -> State ProgState ()
runInstructionV2 (SetMask newMask) = mask .= newMask
runInstructionV2 (SetValue addr val) =
  do
    addresses <- applyMaskV2 addr <$> view mask <$> get
    mapM_ ((memory %=) . flip IM.insert val) addresses

applyMaskV2 :: Int -> [Maybe Bool] -> [Int]
applyMaskV2 = ifoldlM $ \i x -> \case
  Nothing -> [x `clearBit` i, x `setBit` i]
  Just False -> [x]
  Just True -> [x `setBit` i]

execProgram :: (Instruction -> State ProgState ()) -> [Instruction] -> Int
execProgram runInstruction instructions =
  execState (mapM_ runInstruction instructions) initialState ^. memory & sum

initialState :: ProgState
initialState = ProgState [] IM.empty
