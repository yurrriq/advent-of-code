{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day07
  ( manifold,
    getExample,
    example,
    getInput,
    partOne,
    partTwo,
    main,
  )
where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import Control.Lens (ifoldl', makeLenses, use, uses, view, views, (%=), (%~), (+=), (.=), (.~), _1, _2, _3)
import Data.Array ((!))
import Data.Graph (Graph, Vertex, graphFromEdges, reverseTopSort)
import Data.IntMap.Strict qualified as IntMap
import Data.List.Extra (sumOn')
import Data.Set qualified as Set
import Linear (V2 (..), _y)
import Relude
import Text.Show qualified
import Text.Trifecta (Parser, char, choice, newline, sepEndBy)

data Location
  = EmptySpace
  | Splitter
  | Beam

data Manifold
  = Manifold
  { _bounds :: V2 Int,
    _beams :: Set (V2 Int),
    _splitters :: Set (V2 Int)
  }
  deriving (Eq, Generic)

makeLenses ''Manifold

instance Show Manifold where
  show diagram =
    let (V2 maxX maxY) = view bounds diagram
     in [ [ if
              | diagram & views beams (Set.member location) -> '|'
              | diagram & views splitters (Set.member location) -> '^'
              | otherwise -> '.'
          | x <- [0 .. maxX],
            let location = V2 x y
          ]
        | y <- [0 .. maxY]
        ]
          & intercalate "\n"

manifold :: Parser Manifold
manifold =
  ifoldl' (ifoldl' . go) (Manifold 0 Set.empty Set.empty)
    <$> some (choice [EmptySpace <$ char '.', Splitter <$ char '^', Beam <$ char 'S'])
    `sepEndBy` newline
  where
    go y x diagram = \case
      EmptySpace ->
        diagram
          & (bounds .~ V2 x y)
      Splitter ->
        diagram
          & (bounds .~ V2 x y)
          & (splitters %~ Set.insert (V2 x y))
      Beam ->
        diagram
          & (bounds .~ V2 x y)
          & (beams .~ Set.singleton (V2 x y))

getExample :: IO Manifold
getExample = parseString manifold example

example :: String
example =
  ".......S.......\n\
  \...............\n\
  \.......^.......\n\
  \...............\n\
  \......^.^......\n\
  \...............\n\
  \.....^.^.^.....\n\
  \...............\n\
  \....^.^...^....\n\
  \...............\n\
  \...^.^...^.^...\n\
  \...............\n\
  \..^...^.....^..\n\
  \...............\n\
  \.^.^.^.^.^...^.\n\
  \..............."

partOne :: Puzzle Manifold (Int, Int, Manifold) Int
partOne = do
  (_3 .=) =<< ask
  go
  where
    go = do
      done <- uses (_3 . bounds . _y) =<< uses _2 (>)
      if done
        then use _1
        else do
          isTip <- uses _2 (==)
          tips <- uses (_3 . beams) (Set.filter (views _y isTip))
          forM_ tips \beam -> do
            let next = beam + V2 0 1
            reachedSplitter <- uses (_3 . splitters) (Set.member next)
            if reachedSplitter
              then do
                _1 += 1
                (_3 . beams) %= Set.insert (next - V2 1 0) . Set.insert (next + V2 1 0)
              else
                (_3 . beams) %= Set.insert next
          _2 += 1
          go

getInput :: IO Manifold
getInput = parseInputAoC 2025 7 manifold

buildGraph :: Manifold -> (Graph, Vertex -> (V2 Int, V2 Int, [V2 Int]), V2 Int -> Maybe Vertex)
buildGraph Manifold {..} =
  graphFromEdges [(loc, loc, neighbors loc) | loc <- Set.toList _beams]
  where
    neighbors location
      | down `Set.member` _beams = [down]
      | otherwise =
          [ location + delta
          | down `Set.member` _splitters,
            delta <- [V2 (-1) 1, V2 1 1]
          ]
      where
        down = location + V2 0 1

countDagPaths :: Graph -> Vertex -> [Vertex] -> Int
countDagPaths g start ends = foldl' go IntMap.empty (reverseTopSort g) IntMap.! start
  where
    endSet = Set.fromList ends
    go pathCounts location = IntMap.insert location n pathCounts
      where
        n
          | location `Set.member` endSet = 1
          | otherwise = sumOn' (pathCounts IntMap.!) (g ! location)

partTwo :: Puzzle Manifold (Int, Int, Manifold) Int
partTwo = do
  maxY <- use (_3 . bounds . _y)
  (g, _nodeFromVertex, vertexFromKey) <- uses _3 buildGraph
  [start] <- asks (mapMaybe vertexFromKey . Set.toList . _beams)
  uses (_3 . beams) Set.toList
    <&> countDagPaths g start
    . mapMaybe vertexFromKey
    . filter (views _y (== maxY))

main :: IO ()
main = do
  diagram <- getInput
  evalPuzzle diagram (0, 0, diagram) do
    putStr "Part One: "
    print =<< partOne
    putStr "Part Two: "
    print =<< partTwo
