{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2020.Day14.Types where

import AdventOfCode.Puzzle
import Control.Lens (makeLenses)
import Generic.Data (Generically (..))
import Relude
import Text.Show qualified

data ProgState = ProgState
  { _mask :: [Maybe Bool],
    _memory :: IntMap Int
  }
  deriving (Eq, Generic)
  deriving (Semigroup, Monoid) via (Generically ProgState)

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

type Program = Puzzle [Instruction] ProgState
