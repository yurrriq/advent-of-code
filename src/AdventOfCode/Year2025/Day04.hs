{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day04 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (fixM, neighborsOf, (<.>))
import Control.Lens (ifoldMap')
import Data.Set qualified as Set
import Linear (V2 (..))
import Relude
import Text.Trifecta (Parser, char, newline, sepEndBy)

diagram :: Parser (Set (V2 Int))
diagram =
  fmap (ifoldMap' (ifoldMap' . go))
    $ some (True <$ char '@' <|> char '.' $> False)
    `sepEndBy` newline
  where
    go y x = bool Set.empty (Set.singleton (V2 x y))

getExample :: IO (Set (V2 Int))
getExample = parseString diagram example

example :: String
example =
  "..@@.@@@@.\n\
  \@@@.@.@.@@\n\
  \@@@@@.@.@@\n\
  \@.@@@@..@.\n\
  \@@.@@@@.@@\n\
  \.@@@@@@@.@\n\
  \.@.@.@.@@@\n\
  \@.@@@.@@@@\n\
  \.@@@@@@@@.\n\
  \@.@.@@@.@."

getInput :: IO (Set (V2 Int))
getInput = parseInputAoC 2025 4 diagram

canAccess :: V2 Int -> SimplePuzzle (Set (V2 Int)) Bool
canAccess roll = asks \rolls ->
  Set.size (Set.intersection rolls (neighborsOf roll)) < 4

partOne :: SimplePuzzle (Set (V2 Int)) Int
partOne =
  ask
    >>= getSum
    <.> foldMapM (fmap (Sum . fromEnum) . canAccess)

partTwo :: SimplePuzzle (Set (V2 Int)) Int
partTwo = (-) <$> asks Set.size <*> (Set.size <.> fixM removeRolls =<< ask)
  where
    removeRolls rolls = foldlM removeRoll rolls rolls
      where
        removeRoll remaining roll =
          bool remaining (Set.delete roll remaining)
            <$> withPuzzle (const rolls) (canAccess roll)

main :: IO ()
main = $(defaultMainPuzzle)
