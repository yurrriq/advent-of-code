module AdventOfCode.Year2020.Day11 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (fix')
import Control.Applicative ((<|>))
import Data.Bool (bool)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear.V2 (V2 (..))
import Text.Parser.Token.Highlight (Highlight (..))
import Text.Trifecta ((<?>), Parser, char, highlight, newline, sepEndBy, some)
import Prelude hiding (floor)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne input

getInput :: IO (Vector (Vector Position))
getInput = parseInput seatLayout $(inputFilePath)

-- getInput =
--   do
--     Just res <- parseFromFile seatLayout "../../../input/2020/day11.txt"
--     pure res

partOne :: Vector (Vector Position) -> Int
partOne xxs = Map.size $ Map.filter id $ fix' (musicalChairs 4 neighborhood) grid
  where
    neighborhood = neighborsMap (Map.keysSet grid)
    grid = toGrid xxs

neighborsMap :: Set Point -> Map Point (Set Point)
neighborsMap ps = flip Map.fromSet ps $ Set.intersection ps . neighborsOf
  where
    neighborsOf = Set.fromList . flip map adjacencies . (+)

adjacencies :: [Point]
adjacencies =
  [ V2 dx dy
    | dx <- [-1 .. 1],
      dy <- [-1 .. 1],
      not (dx == 0 && dy == 0)
  ]

-- TODO: Control.Lens tricks with indexed
toGrid :: Vector (Vector Position) -> Map (V2 Int) Bool
toGrid xxs = foldr rowIter Map.empty [0 .. height - 1]
  where
    rowIter y z = foldr (columnIter y) z [0 .. width - 1]
    columnIter y x = Map.alter go (V2 x y)
      where
        go _ = case ((xxs V.! y) V.! x) of
          Floor -> Nothing
          EmptySeat -> Just False
          OccupiedSeat -> Just True
    width = length (V.head xxs)
    height = length xxs

musicalChairs :: Int -> Map Point (Set Point) -> Map Point Bool -> Map Point Bool
musicalChairs n neighborhood layout = Map.intersectionWith go neighborhood layout
  where
    go neighbors = bool lonely crowded
      where
        lonely = not (any (layout Map.!) neighbors)
        crowded = length (Set.filter (layout Map.!) neighbors) < n

type Point = V2 Int

data Position
  = Floor
  | EmptySeat
  | OccupiedSeat
  deriving (Eq)

instance Show Position where
  show Floor = "."
  show EmptySeat = "L"
  show OccupiedSeat = "#"

seatLayout :: Parser (Vector (Vector Position))
seatLayout = V.fromList . map V.fromList <$> some position `sepEndBy` newline

position :: Parser Position
position = highlight Symbol $ floor <|> emptySeat <|> occupiedSeat
  where
    floor = char '.' *> pure Floor <?> "floor"
    emptySeat = char 'L' *> pure EmptySeat <?> "an empty seat"
    occupiedSeat = char '#' *> pure OccupiedSeat <?> "an occupied seat"
