{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day10 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util ((<.>))
import Control.Lens (ifoldl', makeLenses, view, views)
import Data.Bits (clearBit, setBit)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NE
import Data.Sequence (Seq ((:<|)), (><))
import Data.Sequence qualified as Seq
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Relude
import Text.Trifecta (Parser, braces, brackets, char, commaSep, natural, parens)

data Machine
  = Machine
  { _lightDiagram :: Int,
    _wiringSchematics :: NonEmpty Int,
    _joltageRequirements :: Vector Int
  }
  deriving (Eq, Generic, Show)

makeLenses ''Machine

machine :: Parser Machine
machine = Machine <$> diagram <*> schematics <*> joltages
  where
    diagram =
      ifoldl' (\i acc b -> bool clearBit setBit b acc i) 0
        <$> brackets (some (False <$ char '.' <|> char '#' $> True))
    schematics = NE.some1 (parens (foldl' setBit 0 <$> commaSep int))
    joltages = Vector.fromList <$> braces (commaSep int)
    int = fromInteger <$> natural

getExample :: IO [Machine]
getExample = parseString (some machine) example

example :: String
example =
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n\
  \[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n\
  \[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

getInput :: IO [Machine]
getInput = parseInputAoC 2025 10 (some machine)

partOne :: SimplePuzzle [Machine] Int
partOne = sum <.> mapM go =<< ask
  where
    go maskin = withPuzzle (const maskin) do
      buttons <- views wiringSchematics NE.toList
      target <- view lightDiagram
      let bfs _visited Seq.Empty = fail "could not correctly configure the indicator lights"
          bfs visited ((lights, n) :<| rest)
            | lights == target = pure n
            | IntSet.member lights visited = bfs visited rest
            | otherwise =
                bfs (IntSet.insert lights visited)
                  $ rest
                  >< Seq.fromList [(lights `xor` button, n + 1) | button <- buttons]
      bfs IntSet.empty (Seq.singleton (0, 0))

partTwo :: SimplePuzzle [Machine] Int
partTwo = fail "not yet implemented"

main :: IO ()
main = $(defaultMainPuzzle)
