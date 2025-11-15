{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2024.Day01 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle (Puzzle)
import AdventOfCode.TH (evalPuzzle)
import AdventOfCode.Util (frequencies)
import Control.Foldl qualified as Foldl
import Control.Lens (makeLenses, (<.=))
import Data.Map qualified as Map
import Relude
import Text.Trifecta (count, natural)

data PuzzleState
  = PuzzleState
  { _answerOne :: !Integer,
    _answerTwo :: !Integer
  }
  deriving (Eq, Generic, Show)

makeLenses ''PuzzleState

emptyPuzzleState :: PuzzleState
emptyPuzzleState = PuzzleState 0 0

main :: IO ()
main = $(evalPuzzle)

partOne :: Puzzle ([Integer], [Integer]) PuzzleState Integer
partOne = do
  (xs, ys) <- ask
  answerOne <.= Foldl.fold (Foldl.premap abs Foldl.sum) (ZipList (zipWith subtract (sort xs) (sort ys)))

partTwo :: Puzzle ([Integer], [Integer]) PuzzleState Integer
partTwo = do
  (xs, ys) <- ask
  let go x = x * toInteger (Map.findWithDefault 0 x (frequencies ys))
  answerTwo <.= Foldl.fold (Foldl.premap go Foldl.sum) xs

getInput :: IO ([Integer], [Integer])
getInput = do
  [xs, ys] <- transpose <$> parseInputAoC 2024 1 (some (count 2 natural))
  pure (xs, ys)
