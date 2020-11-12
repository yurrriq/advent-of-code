module AdventOfCode.Year2015.Day06.Types where

import Foreign.Marshal.Utils (fromBool)
import Linear.V2 (V2 (..))

data Instruction
  = Instruction Operation (Location, Location)
  deriving (Eq, Show)

data Operation
  = TurnOn
  | TurnOff
  | Toggle
  deriving (Eq, Show)

type Location = V2 Int

class Light a where
  off :: a
  alter :: Operation -> a -> a
  brightness :: a -> Int

newtype SimpleLight = SimpleLight Bool

instance Light SimpleLight where
  off = SimpleLight False
  alter TurnOn _ = SimpleLight True
  alter TurnOff _ = SimpleLight False
  alter Toggle (SimpleLight state) = SimpleLight (not state)
  brightness (SimpleLight state) = fromBool state

newtype Dimmer = Dimmer Int

instance Light Dimmer where
  off = Dimmer 0
  alter TurnOn (Dimmer state) = Dimmer (state + 1)
  alter TurnOff (Dimmer state) = Dimmer (max 0 (state - 1))
  alter Toggle (Dimmer state) = Dimmer (state + 2)
  brightness (Dimmer state) = state
