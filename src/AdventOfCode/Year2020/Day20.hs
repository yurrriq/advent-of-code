module AdventOfCode.Year2020.Day20
  ( main,
    getInput,
    partOne,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (frequencies)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Lens (ifoldl', view, (+~), (-~))
import Control.Monad (ap, guard)
import Data.Bool (bool)
import Data.Foldable (toList)
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Linear.V2 (R1 (..), R2 (..), V2 (..), perp, _yx)
import Text.Trifecta (Parser, char, natural, newline, sepEndBy, some, symbol)

type Image = Map Coords LabeledTile

type LabeledTile = (Int, Tile)

type Tile = Set Coords

type Coords = V2 Int

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print (partOne input)

getInput :: IO [LabeledTile]
getInput = parseInput (tile `sepEndBy` newline) $(inputFilePath)

partOne :: [LabeledTile] -> Int
partOne pieces =
  product . map fst . corners . head $
    arrange edges Map.empty (Set.fromList pieces) allHoles
  where
    edges = findEdges pieces
    allHoles = [V2 x y | x <- [0 .. 11], y <- [0 .. 11]]

arrange :: Set [Int] -> Image -> Set LabeledTile -> [Coords] -> [Image]
arrange _ layout _ [] = [layout]
arrange edges layout pieces (hole : holes) =
  do
    ((n, piece), remainingPieces) <- choices pieces
    orientedTile <- orientations piece
    let theTopEdge = topEdge orientedTile
    guard $
      maybe
        (normalizeEdge theTopEdge `Set.member` edges)
        ((theTopEdge ==) . bottomEdge . snd)
        $ layout !? (_y -~ 1) hole
    let theLeftEdge = leftEdge orientedTile
    guard $
      maybe
        (normalizeEdge theLeftEdge `Set.member` edges)
        ((theLeftEdge ==) . rightEdge . snd)
        $ layout !? (_x -~ 1) hole
    arrange edges (Map.insert hole (n, orientedTile) layout) remainingPieces holes

findEdges :: [LabeledTile] -> Set [Int]
findEdges =
  Map.keysSet
    . Map.filter (== 1)
    . frequencies
    . concatMap (normalizedEdges . snd)

orientations :: Tile -> [Tile]
orientations = concatMap flips . rotations

rotations :: Tile -> [Tile]
rotations xys = take 4 (iterate (Set.map ((_x +~ xshift) . perp)) xys)
  where
    xshift = maximum (Set.map (view _x) xys)

flips :: Tile -> [Tile]
flips = take 2 . iterate (Set.map (view _yx))

corners :: Image -> [LabeledTile]
corners = flip map [pure 0, V2 11 0, pure 11, V2 0 11] . (!)

choices :: Set a -> [(a, Set a)]
choices xs =
  [(Set.elemAt i &&& Set.deleteAt i) xs | i <- [0 .. Set.size xs - 1]]

normalizedEdges :: Tile -> [[Int]]
normalizedEdges points =
  map normalizeEdge $
    [topEdge, rightEdge, bottomEdge, leftEdge] <*> pure points

normalizeEdge :: [Int] -> [Int]
normalizeEdge = min `ap` (reverse . map (9 -))

topEdge, rightEdge, bottomEdge, leftEdge :: Tile -> [Int]
topEdge xys = [x | V2 x 0 <- toList xys]
rightEdge xys = [y | V2 9 y <- toList xys]
bottomEdge xys = [x | V2 x 9 <- toList xys]
leftEdge xys = [y | V2 0 y <- toList xys]

tile :: Parser LabeledTile
tile = (,) <$> label <*> grid
  where
    grid = ifoldl' (ifoldl' . go) Set.empty <$> some pixel `sepEndBy` newline
    go y x = bool `ap` Set.insert (V2 x y)

pixel :: Parser Bool
pixel =
  True <$ char '#'
    <|> False <$ char '.'

label :: Parser Int
label = symbol "Tile" *> nonnegInt <* symbol ":"

nonnegInt :: Parser Int
nonnegInt = fromInteger <$> natural
