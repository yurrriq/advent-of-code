{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day07 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import Control.Lens (ifoldl', makeLenses, use, uses, views, (%=), (%~), (+=), (.=), (.~), _1, _2, _3)
import Data.Set qualified as Set
import Linear (V2 (..), _y)
import Relude
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
  deriving (Eq, Generic, Show)

makeLenses ''Manifold

manifold :: Parser Manifold
manifold =
  ifoldl' (ifoldl' . go) (Manifold 0 Set.empty Set.empty)
    <$> some (choice [EmptySpace <$ char '.', Splitter <$ char '^', Beam <$ char 'S'])
    `sepEndBy` newline
  where
    go y x diagram = \case
      EmptySpace ->
        diagram & bounds .~ V2 x y
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

main :: IO ()
main = do
  diagram <- getInput
  evalPuzzle diagram (0, 0, diagram) do
    putStr "Part One: "
    print =<< partOne
