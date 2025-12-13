{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2020.Day14 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Year2020.Day14.Parsers
import AdventOfCode.Year2020.Day14.Types
import Control.Lens (ifoldl', ifoldlM, modifying, uses, (%=), (.=))
import Data.Bits (clearBit, setBit)
import Data.IntMap qualified as IM
import Relude

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO [Instruction]
getInput = parseInputAoC 2020 14 (some instruction)

partOne :: Program Int
partOne = execProgram runInstruction
  where
    runInstruction (SetMask newMask) = mask .= newMask
    runInstruction (SetValue addr val) =
      uses mask (applyMask val)
        >>= modifying memory
        . IM.insert addr

    applyMask = ifoldl' \i x -> \case
      Nothing -> x
      Just False -> x `clearBit` i
      Just True -> x `setBit` i

partTwo :: Program Int
partTwo = execProgram runInstruction
  where
    runInstruction (SetMask newMask) = mask .= newMask
    runInstruction (SetValue encodedAddress val) =
      uses mask (applyMask encodedAddress) >>= mapM_ \addr -> do
        memory %= IM.insert addr val

    applyMask = ifoldlM \i x -> \case
      Nothing -> [x `clearBit` i, x `setBit` i]
      Just False -> [x]
      Just True -> [x `setBit` i]

execProgram :: (Instruction -> Program ()) -> Program Int
execProgram runInstruction = do
  put mempty
  ask >>= mapM_ runInstruction
  uses memory sum
