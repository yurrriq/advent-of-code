{-# LANGUAGE MultiParamTypeClasses #-}

module AdventOfCode.Year2020.Day14.Types where

import Control.Lens (makeLenses)
import Data.IntMap (IntMap)

data ProgState
  = ProgState
      { _mask :: [Maybe Bool],
        _memory :: IntMap Int
      }

makeLenses ''ProgState

data Instruction
  = SetMask [Maybe Bool]
  | SetValue Int Int

instance Show Instruction where
  show (SetMask bits) =
    "mask = "
      <> [ case bit of
             Nothing -> 'X'
             Just True -> '1'
             Just False -> '0'
           | bit <- reverse bits
         ]
  show (SetValue addr val) = "mem[" <> show addr <> "] = " <> show val
