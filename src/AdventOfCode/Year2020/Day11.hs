module AdventOfCode.Year2020.Day11 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (adjacencies, fix', neighborsOf)
import Control.Applicative ((<|>))
import Data.Bool (bool)
import Data.Ix (inRange)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear (V2 (..))
import Text.Parser.Token.Highlight (Highlight (..))
import Text.Trifecta ((<?>), Parser, char, highlight, newline, sepEndBy, some)
import Prelude hiding (floor)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne input
    putStr "Part Two: "
    print $ partTwo input

getInput :: IO (Vector (Vector Position))
getInput = parseInput seatLayout $(inputFilePath)

partOne :: Vector (Vector Position) -> Int
partOne xxs = solve 4 neighborhood grid
  where
    neighborhood = neighborsMap (Map.keysSet grid)
    neighborsMap ps = flip Map.fromSet ps $ Set.intersection ps . neighborsOf
    grid = toGrid xxs

partTwo :: Vector (Vector Position) -> Int
partTwo xxs = solve 5 neighborhood grid
  where
    neighborhood = neighborsMap (Map.keysSet grid)
    neighborsMap ps = flip Map.fromSet ps $ Set.intersection ps . neighborsOf'
      where
        neighborsOf' p =
          Set.fromList $ mapMaybe (firstNeighborInLine p) adjacencies
        firstNeighborInLine p dxy =
          find (`Set.member` ps)
            $ takeWhile (all inRange')
            $ tail (iterate (+ dxy) p)
    -- TODO: be smarter, or (something like) the Grid type from 2019.10
    inRange' = inRange (0, max (length xxs) (length (V.head xxs)))
    grid = toGrid xxs

solve :: Int -> Map Point (Set Point) -> Map Point Bool -> Int
solve n neighborhood =
  Map.size . Map.filter id
    . fix' (musicalChairs n neighborhood)

-- TODO: Control.Lens tricks with indexed
toGrid :: Vector (Vector Position) -> Map (V2 Int) Bool
toGrid xxs = foldr rowIter Map.empty [0 .. height - 1]
  where
    rowIter y z = foldr (columnIter y) z [0 .. width - 1]
    columnIter y x = Map.alter go (V2 x y)
      where
        go _ = case (xxs V.! y) V.! x of
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
