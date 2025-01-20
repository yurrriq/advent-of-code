{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}

module AdventOfCode.Year2024.Day06 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (CyclicEnum (..))
import Control.Applicative ((<|>))
import Control.Lens (ifoldl')
import Data.Ix (inRange)
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V2 (..))
import Text.Trifecta (Parser, char, newline, sepEndBy, some)
import Prelude hiding (Either (..))

data Heading
  = Up
  | Right
  | Down
  | Left
  deriving (Eq, Show, Enum, Bounded, CyclicEnum)

data Position where
  Guard :: Heading -> Position
  Obstacle :: Position
  Empty :: Position
  deriving (Eq, Show)

main :: IO ()
main = $(defaultMain)

partOne :: (V2 Int, (V2 Int, Heading), Set (V2 Int)) -> Int
partOne = Set.size . go Set.empty
  where
    go seen (bounds, theGuard@(position, _), obstacles)
      | inRange (pure 0, bounds - pure 1) position =
          go (Set.insert position seen) (bounds, move theGuard obstacles, obstacles)
      | otherwise = seen

partTwo :: (V2 Int, (V2 Int, Heading), Set (V2 Int)) -> Int
partTwo = undefined

move :: (Ord a, Num a) => (V2 a, Heading) -> Set (V2 a) -> (V2 a, Heading)
move (position, heading) obstacles =
  if facingObstacle
    then (position, csucc heading)
    else (move' (position, heading), heading)
  where
    facingObstacle = Set.member (move' (position, heading)) obstacles

move' :: (Num a) => (V2 a, Heading) -> V2 a
move' (position, heading) =
  case heading of
    Up -> position + V2 0 (-1)
    Right -> position + V2 1 0
    Down -> position + V2 0 1
    Left -> position + V2 (-1) 0

getInput :: IO (V2 Int, (V2 Int, Heading), Set (V2 Int))
getInput = parseInput situationMap $(inputFilePath)

situationMap :: Parser (V2 Int, (V2 Int, Heading), Set (V2 Int))
situationMap =
  mkSituationMap <$> (some position `sepEndBy` newline)
  where
    position =
      (Empty <$ char '.')
        <|> (Obstacle <$ char '#')
        <|> (Guard Up <$ char '^')
        <|> (Guard Down <$ char 'v')
        <|> (Guard Left <$ char '<')
        <|> (Guard Right <$ char '>')

mkSituationMap :: [[Position]] -> (V2 Int, (V2 Int, Heading), Set (V2 Int))
mkSituationMap positions = (V2 width height, theGuard', obstacles')
  where
    (theGuard', obstacles') = ifoldl' (ifoldl' . go) ((pure 0, Up), Set.empty) positions
    go y x (_, obstacles) (Guard heading) = ((V2 x y, heading), obstacles)
    go y x (theGuard, obstacles) Obstacle = (theGuard, Set.insert (V2 x y) obstacles)
    go _ _ acc Empty = acc
    height = length positions
    width = length (head positions)

getExample :: IO (V2 Int, (V2 Int, Heading), Set (V2 Int))
getExample = parseString situationMap example

example :: String
example =
  "....#.....\n\
  \.........#\n\
  \..........\n\
  \..#.......\n\
  \.......#..\n\
  \..........\n\
  \.#..^.....\n\
  \........#.\n\
  \#.........\n\
  \......#..."
