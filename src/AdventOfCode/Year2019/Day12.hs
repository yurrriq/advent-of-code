module AdventOfCode.Year2019.Day12 where

import AdventOfCode.Util (parseInput)
import Control.Arrow ((&&&), (>>>), second)
import Data.Function (on)
import Linear.V3 (V3 (..))
import Text.Trifecta (Parser, angles, between, integer, some, string)

partOne :: IO Int
partOne = sum . fmap totalEnergy . (!! 1000) . iterate step <$> parseMoons

partTwo :: IO Int
partTwo = foldr lcm 1 . fmap period . traverse transpose <$> parseMoons

parseMoons :: IO [Pair]
parseMoons = fmap mkPair <$> parseInput (some dimensions) "input/2019/day12.txt"

type Pair = (Dimensions, Dimensions)

mkPair :: V3 Int -> Pair
mkPair = flip (,) (V3 0 0 0)

type Dimensions = V3 Int

dimensions :: Parser Dimensions
dimensions =
  angles $
    V3
      <$> (fromIntegral <$> between (string "x=") (string ", ") integer)
      <*> (fromIntegral <$> between (string "y=") (string ", ") integer)
      <*> (fromIntegral <$> (string "z=" *> integer))

transpose :: (V3 Int, V3 Int) -> V3 (Int, Int)
transpose (V3 x y z, V3 u v w) = V3 (x, u) (y, v) (z, w)

period :: (Eq a, Num a) => [(a, a)] -> Int
period initial =
  1 + length (takeWhile (/= initial) (iterate step (step initial)))

step :: Num a => [(a, a)] -> [(a, a)]
step = fmap applyVelocity . applyGravities

applyGravities :: Num a => [(a, a)] -> [(a, a)]
applyGravities moons = fmap (flip applyGravity moons) moons

applyGravity :: Num a => (a, a) -> [(a, a)] -> (a, a)
applyGravity = foldr stepVelocity
  where
    stepVelocity there = second =<< (pull `on` fst) there
    pull there here = (+ signum (there - here))

applyVelocity :: Num a => (a, a) -> (a, a)
applyVelocity (pos, vel) = (pos + vel, vel)

totalEnergy :: Pair -> Int
totalEnergy = potentialEnergy &&& kineticEnergy >>> uncurry (*)

potentialEnergy :: Pair -> Int
potentialEnergy = sum . abs . fst

kineticEnergy :: Pair -> Int
kineticEnergy = sum . abs . snd
