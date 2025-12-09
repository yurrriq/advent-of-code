{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day08 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Control.Lens (makeLenses, use, uses, view, (%=), (.=), (<~))
import Data.Bifoldable (biproduct)
import Data.Set qualified as Set
import Generic.Data (Generically (..))
import Linear (Metric, V3 (..), qd, _x)
import Relude
import Relude.Extra.Bifunctor (bimapBoth)
import Text.Trifecta (Parser, comma, decimal, newline, sepEndBy)

getExample :: IO (Set (V3 Int))
getExample = parseString junctionBoxPositions example

junctionBoxPositions :: Parser (Set (V3 Int))
junctionBoxPositions = Set.fromList <$> (go `sepEndBy` newline)
  where
    go = V3 <$> (coordinate <* comma) <*> (coordinate <* comma) <*> coordinate
    coordinate = fromInteger <$> decimal

example :: String
example =
  "162,817,812\n\
  \57,618,57\n\
  \906,360,560\n\
  \592,479,940\n\
  \352,342,300\n\
  \466,668,158\n\
  \542,29,236\n\
  \431,825,988\n\
  \739,650,466\n\
  \52,470,668\n\
  \216,146,977\n\
  \819,987,18\n\
  \117,168,530\n\
  \805,96,715\n\
  \346,949,466\n\
  \970,615,88\n\
  \941,993,340\n\
  \862,61,35\n\
  \984,92,344\n\
  \425,690,689"

getInput :: IO (Set (V3 Int))
getInput = parseInputAoC 2025 8 junctionBoxPositions

-- TODO: KD tree? VP tree?

nearestPairs :: (Metric f, Num a, Ord a) => [f a] -> [(f a, f a)]
nearestPairs pts = sortOn (uncurry qd) [(p, q) | p : ps <- tails pts, q <- ps]

data PuzzleState a
  = PuzzleState
  { _circuits :: Set (Set a),
    _junctionPairs :: [(a, a)]
  }
  deriving (Eq, Generic, Show)
  deriving
    (Semigroup, Monoid)
    via (Generically (PuzzleState a))

makeLenses ''PuzzleState

initPuzzleState :: Puzzle (Set (V3 Int)) (PuzzleState (V3 Int)) ()
initPuzzleState = do
  circuits <~ asks (Set.map Set.singleton)
  junctionPairs <~ asks (nearestPairs . Set.toList)

connect :: (Ord a) => Puzzle (Set a) (PuzzleState a) (a, a)
connect = do
  (from, to) : pairs <- use junctionPairs
  junctionPairs .= pairs
  Just fromCircuit <- uses circuits (find (from `Set.member`))
  when (to `Set.notMember` fromCircuit) $ do
    Just toCircuit <- uses circuits (find (to `Set.member`))
    circuits %= Set.insert (fromCircuit <> toCircuit) . Set.delete fromCircuit . Set.delete toCircuit
  pure (from, to)

partOne :: SimplePuzzle (Set (V3 Int)) Int
partOne =
  evaluatingPuzzleM do
    initPuzzleState
    replicateM 1000 connect
      *> uses circuits (product . take 3 . Set.toDescList . Set.map Set.size)

partTwo :: SimplePuzzle (Set (V3 Int)) Int
partTwo =
  evaluatingPuzzleM do
    initPuzzleState
    loop <&> biproduct . bimapBoth (view _x)
  where
    loop = do
      lastPair <- connect
      ifM (uses circuits ((1 <) . Set.size)) loop
        $ pure lastPair

main :: IO ()
main = $(defaultMainPuzzle)
