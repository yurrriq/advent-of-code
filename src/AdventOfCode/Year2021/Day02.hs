module AdventOfCode.Year2021.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Lens ((+~), (-~))
import Data.Foldable (foldl')
import Data.Functor (($>))
import Linear (V2 (..), _x, _y)
import Text.Trifecta (Parser, many, natural, symbol)

data Command a
  = Cmd Direction a
  deriving (Eq, Show)

data Direction
  = Forward
  | Down
  | Up
  deriving (Eq, Show)

main :: IO ()
main = $(defaultMain)

getInput :: IO [Command Integer]
getInput = parseInput (many command) $(inputFilePath)

example :: [Command Integer]
example =
  [ Cmd Forward 5,
    Cmd Down 5,
    Cmd Forward 8,
    Cmd Up 3,
    Cmd Down 8,
    Cmd Forward 2
  ]

partOne :: [Command Integer] -> Integer
partOne = product . foldl' (flip runCommand) (pure 0)
  where
    runCommand :: Command Integer -> V2 Integer -> V2 Integer
    runCommand (Cmd Forward n) = _x +~ n
    runCommand (Cmd Down n) = _y +~ n
    runCommand (Cmd Up n) = _y -~ n

partTwo :: [Command Integer] -> Integer
partTwo = undefined

command :: Parser (Command Integer)
command = Cmd <$> direction <*> natural

direction :: Parser Direction
direction =
  symbol "forward" $> Forward
    <|> symbol "down" $> Down
    <|> symbol "up" $> Up
