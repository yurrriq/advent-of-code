{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ParallelListComp #-}

module AdventOfCode.Year2024.Day06 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (CyclicEnum (..))
import Control.Applicative ((<|>))
import Control.Lens (ifoldl', makeLenses, to, (%=), (%~), (&), (&~), (+~), (.=), (.~), (^.))
import Data.Ix (inRange)
import Data.Set (Set)
import Data.Set qualified as Set
import Linear (V2 (..), _x, _y)
import Text.Trifecta (Parser, char, newline, sepEndBy, some)
import Prelude hiding (Either (..))

data Heading
  = Up
  | Right
  | Down
  | Left
  deriving (Eq, Show, Enum, Bounded, CyclicEnum)

data Position where
  TheGuard :: Heading -> Position
  AnObstacle :: Position
  Empty :: Position
  deriving (Eq, Show)

type Coordinates = V2 Int

data Guard = Guard
  { _position :: Coordinates,
    _heading :: Heading
  }
  deriving (Eq, Show)

makeLenses ''Guard

data SituationMap
  = SituationMap
  { _bounds :: Coordinates,
    _guard :: Guard,
    _obstacles :: Set Coordinates
  }
  deriving (Eq)

makeLenses ''SituationMap

instance Show SituationMap where
  show situation =
    unlines
      [ [ if
            | V2 x y == situation ^. guard . position ->
                case situation ^. guard . heading of
                  Up -> '^'
                  Right -> '>'
                  Down -> 'v'
                  Left -> '<'
            | Set.member (V2 x y) (situation ^. obstacles) -> '#'
            | otherwise -> '.'
        | x <- [0 .. situation ^. bounds . _x]
        ]
      | y <- [0 .. situation ^. bounds . _y]
      ]

main :: IO ()
main = $(defaultMain)

partOne :: SituationMap -> Int
partOne = Set.size . go . (Set.empty,)
  where
    go (visited, situation) =
      moveGuard situation & \moved ->
        if guardInBounds moved
          then go (Set.insert (moved ^. guard . position) visited, moved)
          else visited

partTwo :: SituationMap -> Int
-- partTwo situation = count (not . causesParadox situation) candidates
--   where
--     candidates =
--       [ coords
--       | x <- [0 .. situation ^. bounds . _x],
--         y <- [0 .. situation ^. bounds . _y],
--         let coords = V2 x y,
--         coords /= situation ^. guard . position,
--         situation ^. obstacles . to (Set.notMember coords)
--       ]
partTwo = undefined

causesParadox :: SituationMap -> Coordinates -> Bool
causesParadox situation candidate = go (Set.empty, situation & obstacles %~ Set.insert candidate)
  where
    go (visited, situation') =
      moveGuard situation' & \moved ->
        guardInBounds moved
          && ( Set.member (moved ^. guard . position) visited
                 || go (Set.insert (moved ^. guard . position) visited, moved)
             )

guardInBounds :: SituationMap -> Bool
guardInBounds situation =
  situation ^. guard . position . to (inRange (0, situation ^. bounds))

moveGuard :: SituationMap -> SituationMap
moveGuard situation =
  if Set.member (moved ^. position) (situation ^. obstacles)
    then situation & guard . heading %~ csucc & moveGuard
    else situation & guard .~ moved
  where
    moved =
      (situation ^. guard) & \theGuard ->
        theGuard & position +~ (theGuard ^. heading . to headingToCoordinates)

headingToCoordinates :: Heading -> Coordinates
headingToCoordinates = \case
  Up -> V2 0 (-1)
  Right -> V2 1 0
  Down -> V2 0 1
  Left -> V2 (-1) 0

getInput :: IO SituationMap
getInput = parseInput situationMap $(inputFilePath)

situationMap :: Parser SituationMap
situationMap =
  fmap mkSituationMap $
    flip sepEndBy newline $
      some $
        (Empty <$ char '.')
          <|> (AnObstacle <$ char '#')
          <|> (TheGuard Up <$ char '^')
          <|> (TheGuard Down <$ char 'v')
          <|> (TheGuard Left <$ char '<')
          <|> (TheGuard Right <$ char '>')

mkSituationMap :: [[Position]] -> SituationMap
mkSituationMap = ifoldl' (ifoldl' . go) (SituationMap (pure 0) (Guard (pure 0) Up) Set.empty)
  where
    go y x situation = \case
      TheGuard theHeading ->
        situation
          & bounds .~ V2 x y
          & guard .~ Guard (V2 x y) theHeading
      AnObstacle ->
        situation &~ do
          bounds .= V2 x y
          obstacles %= Set.insert (V2 x y)
      Empty ->
        situation & bounds .~ V2 x y

getExample :: IO SituationMap
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
  \......#...\n"
