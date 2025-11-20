{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2020.Day09
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle, inputFilePath)
import AdventOfCode.Util (maybeFail)
import Control.Foldl qualified as Foldl
import Control.Lens (use, (<.=))
import Data.Bifoldable (bisum)
import Relude
import Text.Trifecta (natural)

type PuzzleState = GPuzzleState1 Int

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO [Int]
getInput = parseInput (some (fromInteger <$> natural)) $(inputFilePath)

partOne :: Puzzle [Int] PuzzleState Int
partOne = do
  numbers <- ask
  answer <-
    maybeFail "ope!"
      $ listToMaybe
      . snd
      =<< find (null . go) (splitAt 25 <$> tails numbers)
  answerOne <.= answer
  where
    go (preamble, z : _) =
      [ (z, x, y)
      | length preamble == 25,
        (x : ys) <- tails preamble,
        x < z,
        y <- ys,
        y < z,
        z == x + y
      ]
    go _ = []

partTwo :: Puzzle [Int] PuzzleState Int
partTwo = do
  n <- use answerOne
  numbers <- ask
  answer <-
    maybeFail "ope!"
      $ fmap bisum
      . bisequence
      . Foldl.fold ((,) <$> Foldl.minimum <*> Foldl.maximum)
      =<< find ((n ==) . sum) (concatMap inits (tails numbers))
  answerTwo <.= answer
