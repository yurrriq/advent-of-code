module AdventOfCode.Year2015.Day06.Parsers where

import AdventOfCode.Year2015.Day06.Types
import Control.Applicative ((<|>))
import Linear.V2 (V2 (..))
import Text.Trifecta

instruction :: Parser Instruction
instruction = Instruction <$> op <*> locationRange
  where
    op =
      pure TurnOn <* token (string "turn on")
        <|> pure TurnOff <* token (string "turn off")
        <|> pure Toggle <* token (string "toggle")

locationRange :: Parser (Location, Location)
locationRange = (,) <$> location <* token (string "through") <*> location

location :: Parser Location
location = V2 <$> int <* comma <*> int
  where
    int = fromInteger <$> natural
