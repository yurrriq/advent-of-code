{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2023.Day02
  ( main,
    partOne,
    partTwo,
    getInput,
    example,
  )
where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.List.Extra (sumOn')
import Data.Traversable (for)
import Linear (V3 (..))
import Relude
import Text.Trifecta hiding (parseString)

data Game
  = Game
  { idNumber :: !Integer,
    revelations :: ![V3 Integer]
  }
  deriving (Eq, Generic, Show)

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle [Game] Integer
partOne = asks (sumOn' idNumber . filter (isPossible . revelations))
  where
    isPossible = all (and . liftA2 (>=) (V3 12 13 14))

partTwo :: SimplePuzzle [Game] Integer
partTwo = asks (sumOn' (power . revelations))
  where
    power = product . foldr (liftA2 max) 0

getInput :: IO [Game]
getInput = parseInputAoC 2023 2 (game `sepEndBy` newline)

game :: Parser Game
game = Game <$> parseIdNumber <*> parseRevelations `sepBy` symbol ";"
  where
    parseIdNumber = symbol "Game" *> natural <* symbol ":"
    parseRevelations = sum <$> revelation `sepBy` comma

revelation :: Parser (V3 Integer)
revelation =
  natural >>= \n ->
    for (V3 "red" "green" "blue") $ \color ->
      n <$ string color <|> pure 0

example :: String
example =
  toString
    . unlines
    $ [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
      ]
