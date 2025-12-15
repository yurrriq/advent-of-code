{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2019.Day03 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.Foldable (maximum, minimum, minimumBy)
import Data.Ix (Ix (..))
import Data.Set qualified as Set
import Linear (V2 (..), (^*))
import Relude
import Relude.Extra.Bifunctor (bimapBoth)
import Text.Trifecta (Parser, char, choice, comma, natural, sepBy)

-- ------------------------------------------------------------------- [ Types ]

data Segment
  = Segment !Direction !Int
  deriving (Eq, Generic, Show)

data Direction
  = D
  | L
  | R
  | U
  deriving (Eq, Generic, Show)

type Point = V2 Int

-- ----------------------------------------------------------------- [ Parsers ]

point :: Parser Point
point = V2 <$> nonnegInt <*> (comma *> nonnegInt)

wires :: Parser ([Segment], [Segment])
wires = (,) <$> segments <*> segments

segments :: Parser [Segment]
segments = segment `sepBy` comma

segment :: Parser Segment
segment = Segment <$> direction <*> nonnegInt

direction :: Parser Direction
direction =
  choice
    [ D <$ char 'D',
      L <$ char 'L',
      R <$ char 'R',
      U <$ char 'U'
    ]

nonnegInt :: Parser Int
nonnegInt = fromIntegral <$> natural

-- ----------------------------------------------------------------- [ Helpers ]

manhattanDistance :: Point -> Point -> Int
manhattanDistance from to = sum (abs (to - from))

findCrossings :: [Point] -> [Point] -> Set Point
findCrossings = Set.intersection `on` Set.fromList

runSegments :: [Segment] -> [Point]
runSegments = snd . foldl' go (0, [])
  where
    go (start, pointses) seg =
      second (pointses ++) (runSegment (start, seg))

runSegment :: (Point, Segment) -> (Point, [Point])
runSegment (from, Segment direktion distance)
  | 1 == maximum (signum delta) = (to, range (from + delta, to))
  | otherwise = (to, reverse (range (to, from + delta)))
  where
    delta = directionToPoint direktion
    to = from + delta ^* distance

directionToPoint :: Direction -> Point
directionToPoint = \case
  D -> V2 0 (-1)
  L -> V2 (-1) 0
  R -> V2 1 0
  U -> V2 0 1

-- ---------------------------------------------------------------- [ Examples ]

exampleOne :: IO ()
exampleOne =
  runExample
    "R8,U5,L5,D3\n\
    \U7,R6,D4,L4"

exampleTwo :: IO ()
exampleTwo =
  runExample
    "R75,D30,R83,U83,L12,D49,R71,U7,L72\n\
    \U62,R66,U55,R34,D71,R55,D58,R83"

exampleThree :: IO ()
exampleThree =
  runExample
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\n\
    \U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

runExample :: String -> IO ()
runExample =
  parseString wires >=> evaluatingPuzzle do
    putStr "Part One: "
    print =<< partOne
    putStr "Part Two: "
    print =<< partTwo

-- ------------------------------------------------------------------- [ Parts ]

partOne :: SimplePuzzle ([Segment], [Segment]) Int
partOne =
  asks
    $ distance
    . minimumBy (compare `on` distance)
    . uncurry (findCrossings `on` runSegments)
  where
    distance = manhattanDistance 0

partTwo :: SimplePuzzle ([Segment], [Segment]) Int
partTwo =
  asks $ bimapBoth runSegments >>> \(xs, ys) ->
    findCrossings xs ys
      & Set.map (\p -> 2 + ((+) `on` length . takeWhile (/= p)) xs ys)
      & minimum

getInput :: IO ([Segment], [Segment])
getInput = parseInputAoC 2019 3 wires

main :: IO ()
main = $(defaultMainPuzzle)
