{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AdventOfCode.Year2021.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Monoid.Action (Action (..))
import Data.Monoid.SemiDirectProduct.Strict (Semi, embed, inject, untag)
import Data.Semigroup (Sum (..))
import Linear (V2 (..))
import Text.Trifecta (Parser, natural, some, symbol)

newtype Direction = Direction {unDirection :: V2 Int}
  deriving stock (Eq, Show)
  deriving
    (Semigroup, Monoid)
    via (Sum (V2 Int))

newtype Aim = Aim Int
  deriving stock (Eq, Show)
  deriving
    (Semigroup, Monoid)
    via (Sum Int)

instance Action Aim Direction where
  act (Aim a) (Direction (V2 x y)) = Direction (V2 x (y + a * x))

main :: IO ()
main = $(defaultMain)

getInput :: IO [Direction]
getInput = parseInput (some direction) $(inputFilePath)

example :: [Direction]
example =
  [ forward 5,
    down 5,
    forward 8,
    up 3,
    down 8,
    forward 2
  ]

partOne :: [Direction] -> Int
partOne = solve unDirection

partTwo :: [Direction] -> Int
partTwo = solve (unDirection . untag) . map phi
  where
    phi :: Direction -> Semi Direction Aim
    phi dir@(Direction (V2 _ 0)) = inject dir
    phi (Direction (V2 0 y)) = embed (Aim y)
    phi _ = error "Invalid direction"

solve :: Monoid m => (m -> V2 Int) -> [m] -> Int
solve extract = product . extract . mconcat

direction :: Parser Direction
direction = dir <*> (fromInteger <$> natural)
  where
    dir =
      symbol "forward" $> forward
        <|> symbol "down" $> down
        <|> symbol "up" $> up

forward, down, up :: Int -> Direction
forward = Direction . flip V2 0
down = Direction . V2 0
up = down . negate
