{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2021.Day02 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util ((<.>))
import Data.Monoid.Action (Action (..))
import Data.Monoid.SemiDirectProduct.Strict (embed, inject, untag)
import Linear (V2 (..))
import Relude
import Text.Trifecta (Parser, choice, natural, symbol)

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
  act (Aim a) (Direction (V2 x y)) =
    Direction (V2 x (y + a * x))

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO [Direction]
getInput = parseInputAoC 2021 2 (some direction)

getExample :: IO [Direction]
getExample =
  pure
    [ forward 5,
      down 5,
      forward 8,
      up 3,
      down 8,
      forward 2
    ]

partOne :: SimplePuzzle [Direction] Int
partOne = asks (solve unDirection)

partTwo :: SimplePuzzle [Direction] Int
partTwo =
  ask >>= solve (unDirection . untag) <.> traverse \case
    dir@(Direction (V2 _ 0)) -> pure (inject dir)
    Direction (V2 0 y) -> pure (embed (Aim y))
    _invalid -> fail "Invalid direction"

solve :: (Monoid m) => (m -> V2 Int) -> [m] -> Int
solve extract = product . extract . mconcat

direction :: Parser Direction
direction = dir <*> (fromInteger <$> natural)
  where
    dir =
      choice
        [ symbol "forward" $> forward,
          symbol "down" $> down,
          symbol "up" $> up
        ]

forward, down, up :: Int -> Direction
forward = Direction . flip V2 0
down = Direction . V2 0
up = down . negate
