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

getExampleOne :: IO (Map String [String])
getExampleOne = parseString devices exampleOne

exampleOne :: String
exampleOne =
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

allPaths :: (Ord a) => a -> a -> (Map a [a] -> Set a) -> Map a [a] -> Set [a]
allPaths start goal reachability graph = go start Set.empty
  where
    reachable = reachability graph
    go v visited
      | Set.notMember v reachable = Set.empty
      | v == goal = Set.singleton [goal]
      | Set.member v visited = Set.empty
      | otherwise =
          Set.unions
            [ Set.map (v :) (go next (Set.insert v visited))
            | next <- Map.findWithDefault [] v graph
            ]

reachableToGoals :: (Ord a) => [a] -> Map a [a] -> Set a
reachableToGoals goals graph = dfs Set.empty goals
  where
    reverseGraph =
      Map.fromListWith
        (++)
        [ (output, [device])
        | (device, outputs) <- Map.toList graph,
          output <- outputs
        ]

    dfs seen [] = seen
    dfs seen (x : xs)
      | Set.member x seen = dfs seen xs
      | otherwise =
          dfs
            (Set.insert x seen)
            (Map.findWithDefault [] x reverseGraph ++ xs)

partOne :: SimplePuzzle (Map String [String]) Int
partOne = asks (Set.size . allPaths "you" "out" (reachableToGoals ["out"]))

getExampleTwo :: IO (Map String [String])
getExampleTwo = parseString devices exampleTwo

exampleTwo :: String
exampleTwo =
  "svr: aaa bbb\n\
  \aaa: fft\n\
  \fft: ccc\n\
  \bbb: tty\n\
  \tty: ccc\n\
  \ccc: ddd eee\n\
  \ddd: hub\n\
  \hub: fff\n\
  \eee: dac\n\
  \dac: fff\n\
  \fff: ggg hhh\n\
  \ggg: out\n\
  \hhh: out"

partTwo :: SimplePuzzle (Map String [String]) Int
partTwo = asks (Set.size . Set.filter go . allPaths "svr" "out" reachability)
  where
    go path = "dac" `elem` path && "fft" `elem` path
    reachability = reachableToGoals ["out", "dac", "fft"]

main :: IO ()
main = $(defaultMainPuzzle)
