{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2024.Day06 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle (Puzzle, runPuzzle)
import AdventOfCode.TH (evalPuzzle)
import AdventOfCode.Util (CyclicEnum (..), count)
import Control.Lens (ifoldl', makeLenses, to, (%=), (%~), (&~), (+~), (.=), (.~), (<.=), (^.))
import Data.Ix (inRange)
import Data.Set qualified as Set
import Linear (V2 (..), _x, _y)
import Relude hiding (Down, Either (..), unlines)
import Text.Show qualified as Show
import Text.Trifecta (Parser, char, newline, sepEndBy)
import Prelude (unlines)

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
  { _position :: !Coordinates,
    _heading :: !Heading
  }
  deriving (Eq, Show)

makeLenses ''Guard

data SituationMap
  = SituationMap
  { _mapBounds :: !Coordinates,
    _mapGuard :: !Guard,
    _mapObstacles :: !(Set Coordinates)
  }
  deriving (Eq)

makeLenses ''SituationMap

instance Show SituationMap where
  show situation =
    unlines
      [ [ if
            | V2 x y == situation ^. mapGuard . position ->
                case situation ^. mapGuard . heading of
                  Up -> '^'
                  Right -> '>'
                  Down -> 'v'
                  Left -> '<'
            | Set.member (V2 x y) (situation ^. mapObstacles) -> '#'
            | otherwise -> '.'
        | x <- [0 .. situation ^. mapBounds . _x]
        ]
      | y <- [0 .. situation ^. mapBounds . _y]
      ]

data PuzzleState
  = PuzzleState
  { _answerOne :: !Int,
    _answerTwo :: !Int
  }
  deriving (Eq, Generic, Show)

makeLenses ''PuzzleState

emptyPuzzleState :: PuzzleState
emptyPuzzleState = PuzzleState 0 0

main :: IO ()
main = $(evalPuzzle)

partOne :: Puzzle SituationMap PuzzleState Int
partOne = do
  situation <- ask
  answerOne <.= Set.size (go (Set.empty, situation))
  where
    go (visited, situation) =
      moveGuard situation & \moved ->
        if guardInBounds moved
          then go (Set.insert (moved ^. mapGuard . position) visited, moved)
          else visited

partTwo :: Puzzle SituationMap PuzzleState Int
partTwo = do
  situation <- ask
  let candidates =
        [ coords
        | x <- [0 .. situation ^. mapBounds . _x],
          y <- [0 .. situation ^. mapBounds . _y],
          let coords = V2 x y,
          coords /= situation ^. mapGuard . position,
          situation ^. mapObstacles . to (Set.notMember coords)
        ]

  answerTwo <.= count (not . causesParadox situation) candidates

causesParadox :: SituationMap -> Coordinates -> Bool
causesParadox situation candidate = go (Set.empty, situation & mapObstacles %~ Set.insert candidate)
  where
    go (visited, situation') =
      moveGuard situation' & \moved ->
        guardInBounds moved
          && ( Set.member (moved ^. mapGuard . position) visited
                 || go (Set.insert (moved ^. mapGuard . position) visited, moved)
             )

guardInBounds :: SituationMap -> Bool
guardInBounds situation =
  situation ^. mapGuard . position . to (inRange (0, situation ^. mapBounds))

moveGuard :: SituationMap -> SituationMap
moveGuard situation =
  if Set.member (moved ^. position) (situation ^. mapObstacles)
    then situation & mapGuard . heading %~ csucc & moveGuard
    else situation & mapGuard .~ moved
  where
    moved =
      (situation ^. mapGuard) & \theGuard ->
        theGuard & position +~ (theGuard ^. heading . to headingToCoordinates)

headingToCoordinates :: Heading -> Coordinates
headingToCoordinates = \case
  Up -> V2 0 (-1)
  Right -> V2 1 0
  Down -> V2 0 1
  Left -> V2 (-1) 0

getInput :: IO SituationMap
getInput = parseInputAoC 2024 6 situationMap

situationMap :: Parser SituationMap
situationMap =
  fmap mkSituationMap
    $ flip sepEndBy newline
    $ some
    $ (Empty <$ char '.')
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
          & mapBounds
          .~ V2 x y
            & mapGuard
          .~ Guard (V2 x y) theHeading
      AnObstacle ->
        situation &~ do
          mapBounds .= V2 x y
          mapObstacles %= Set.insert (V2 x y)
      Empty ->
        situation & mapBounds .~ V2 x y

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
