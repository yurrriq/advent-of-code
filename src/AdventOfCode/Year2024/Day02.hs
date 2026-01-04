{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2024.Day02 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (count, holes)
import Control.Lens ((<.=))
import Data.Ix (inRange)
import Relude
import Relude.Extra.Bifunctor (bimapBoth)
import Text.Trifecta (char, decimal, newline, sepBy1, sepEndBy)

type PuzzleState = GPuzzleState1 Int

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: Puzzle [[Integer]] PuzzleState Int
partOne = do
  reports <- ask
  answerOne <.= count isSafe reports

partTwo :: Puzzle [[Integer]] PuzzleState Int
partTwo = do
  reports <- ask
  answerTwo <.= count (liftA2 (||) isSafe (any (isSafe . snd) . holes)) reports

getInput :: IO [[Integer]]
getInput = parseInputAoC 2024 2 ((decimal `sepBy1` char ' ') `sepEndBy` newline)

example :: [[Integer]]
example =
  [ [7, 6, 4, 2, 1],
    [1, 2, 7, 8, 9],
    [9, 7, 6, 2, 1],
    [1, 3, 2, 4, 5],
    [8, 6, 4, 4, 1],
    [1, 3, 6, 7, 9]
  ]

isSafe :: [Integer] -> Bool
isSafe =
  uncons >>> maybe False \(x, xs) ->
    biany getAll getAll
      $ foldMap (bimapBoth (All . inRange (1, 3)) . (id &&& negate))
      $ zipWith subtract (x : xs) xs
