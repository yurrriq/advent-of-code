module AdventOfCode.Year2023.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Lens (makeLenses, view)
import Linear (V3 (..))
import Text.Trifecta hiding (digit, parseString)
import Prelude hiding (id)

data Game = Game
  { _id :: Integer,
    _revelations :: [V3 Integer]
  }
  deriving (Eq, Show)

makeLenses ''Game

main :: IO ()
main = $(defaultMain)

partOne :: [Game] -> Integer
partOne = sum . map (view id) . filter (isPossible (V3 12 13 14))

partTwo :: [Game] -> Integer
partTwo = undefined

getInput :: IO [Game]
getInput = parseInput (game `sepEndBy` newline) $(inputFilePath)

isPossible :: V3 Integer -> Game -> Bool
isPossible inventory = all (go inventory) . view revelations
  where
    go (V3 r g b) (V3 r' g' b') = r >= r' && g >= g' && b >= b'

game :: Parser Game
game =
  symbol "Game"
    *> ( Game
           <$> decimal <* symbol ":"
           <*> (sum <$> revelation `sepBy` comma) `sepBy` symbol ";"
       )

revelation :: Parser (V3 Integer)
revelation =
  do
    n <- natural
    (V3 n 0 0 <$ string "red")
      <|> (V3 0 n 0 <$ string "green")
      <|> (V3 0 0 n <$ string "blue")

example :: String
example =
  unlines
    [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]
