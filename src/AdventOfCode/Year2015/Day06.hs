-- Thanks to https://github.com/amalloy/advent-of-code/blob/30375f09/day6/src.hs
-- for some inspiration.

module AdventOfCode.Year2015.Day06 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.Trans.State (State, execState, modify)
import Data.Ix (range)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Utils (fromBool)
import Linear (V2 (..))
import Text.Trifecta (Parser, comma, natural, some, symbol)

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

main :: IO ()
main = $(defaultMain)

getInput :: IO [Instruction]
getInput = parseInput (some instruction) $(inputFilePath)

partOne :: [Instruction] -> Int
partOne = howBright (Map.empty :: Map Location SimpleLight)

partTwo :: [Instruction] -> Int
partTwo = howBright (Map.empty :: Map Location Dimmer)

howBright :: (Light a) => Map Location a -> [Instruction] -> Int
howBright emptyGrid instructions =
  foldr ((+) . brightness) 0 $
    execState (mapM_ runInstruction instructions) emptyGrid

runInstruction :: (Light a) => Instruction -> State (Map Location a) ()
runInstruction (Instruction op locations) = forM_ (range locations) $ \loc ->
  modify (Map.alter (Just . alter op . fromMaybe off) loc)

instruction :: Parser Instruction
instruction = Instruction <$> op <*> locationRange
  where
    op =
      TurnOn <$ symbol "turn on"
        <|> TurnOff <$ symbol "turn off"
        <|> Toggle <$ symbol "toggle"

locationRange :: Parser (Location, Location)
locationRange = (,) <$> location <* symbol "through" <*> location

location :: Parser Location
location = V2 <$> int <* comma <*> int
  where
    int = fromInteger <$> natural
