{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day11 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Relude
import Text.Trifecta (Parser, char, lower, newline, sepEndBy, symbol)

devices :: Parser (Map String [String])
devices = Map.fromList <$> device `sepEndBy` newline
  where
    device =
      (,)
        <$> (some lower <* symbol ":")
        <*> (some lower `sepEndBy` char ' ')

getExample :: IO (Map String [String])
getExample = parseString devices example

example :: String
example =
  "aaa: you hhh\n\
  \you: bbb ccc\n\
  \bbb: ddd eee\n\
  \ccc: ddd eee fff\n\
  \ddd: ggg\n\
  \eee: out\n\
  \fff: out\n\
  \ggg: out\n\
  \hhh: ccc fff iii\n\
  \iii: out"

getInput :: IO (Map String [String])
getInput = parseInputAoC 2025 11 devices

allPaths :: (Ord a) => a -> a -> Map a [a] -> Set [a]
allPaths start goal graph = go start Set.empty
  where
    go v visited
      | v == goal = Set.singleton [goal]
      | Set.member v visited = Set.empty
      | otherwise =
          Set.unions
            [ Set.map (v :) (go next (Set.insert v visited))
            | next <- Map.findWithDefault [] v graph
            ]

partOne :: SimplePuzzle (Map String [String]) Int
partOne = asks (Set.size . allPaths "you" "out")

partTwo :: SimplePuzzle (Map String [String]) ()
partTwo = fail "not yet implemented"

main :: IO ()
main = $(defaultMainPuzzle)
