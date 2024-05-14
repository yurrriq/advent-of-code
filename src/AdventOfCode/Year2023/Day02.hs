module AdventOfCode.Year2023.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Data.List.Extra (sumOn')
import Data.Traversable (for)
import Linear (V3 (..))
import Text.Trifecta hiding (parseString)
import Prelude hiding (id)

newtype Game = Game {unGame :: (Integer, [V3 Integer])}

main :: IO ()
main = $(defaultMain)

partOne :: [Game] -> Integer
partOne = sumOn' (fst . unGame) . filter (isPossible . snd . unGame)
  where
    isPossible = all (and . liftA2 (>=) (V3 12 13 14))

partTwo :: [Game] -> Integer
partTwo = sumOn' (power . snd . unGame)
  where
    power = product . foldr (liftA2 max) 0

getInput :: IO [Game]
getInput = parseInput (game `sepEndBy` newline) $(inputFilePath)

game :: Parser Game
game =
  fmap Game $
    symbol "Game"
      *> ( (,)
             <$> natural
             <* symbol ":"
             <*> (sum <$> revelation `sepBy` comma) `sepBy` symbol ";"
         )

revelation :: Parser (V3 Integer)
revelation =
  natural >>= \n ->
    for (V3 "red" "green" "blue") $ \color ->
      n <$ string color <|> pure 0

example :: String
example =
  unlines
    [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]
