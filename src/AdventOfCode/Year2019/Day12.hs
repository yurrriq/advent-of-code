{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2019.Day12 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util ((<.>))
import Data.Foldable.Extra (sumOn')
import Data.List.Infinite ((!!))
import Data.List.Infinite qualified as Infinite
import Linear (V2 (..), V3 (..))
import Relude
import Text.Trifecta (Parser, angles, between, integer, string)

type Moon = V2 (V3 Int)

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO [Moon]
getInput = parseInputAoC 2019 12 (some (flip V2 0 <$> dimensions))

partOne :: SimplePuzzle [Moon] Int
partOne = asks (sumOn' totalEnergy . (!! 1000) . Infinite.iterate step)
  where
    totalEnergy (V2 position velocity) =
      ((*) `on` (sum . abs)) position velocity

partTwo :: SimplePuzzle [Moon] Int
partTwo = asks (foldr (lcm . period) 1 . traverse twist)
  where
    twist (V2 (V3 x y z) (V3 u v w)) = V3 (V2 x u) (V2 y v) (V2 z w)

dimensions :: Parser (V3 Int)
dimensions =
  fmap fromInteger
    <.> angles
    $ V3
    <$> between (string "x=") (string ", ") integer
    <*> between (string "y=") (string ", ") integer
    <*> (string "z=" *> integer)

period :: (Functor f, Foldable f, Eq (f (V2 a)), Num a) => f (V2 a) -> Int
period initial =
  1 + length (Infinite.takeWhile (/= initial) (Infinite.iterate step (step initial)))

step :: (Functor f, Foldable f, Num a) => f (V2 a) -> f (V2 a)
step = fmap applyVelocity . applyGravities
  where
    applyVelocity (V2 position velocity) = V2 (position + velocity) velocity
    applyGravities moons = fmap (`applyGravity` moons) moons
    applyGravity = foldr \(V2 there _) (V2 here velocity) ->
      V2 here (velocity + signum (there - here))
