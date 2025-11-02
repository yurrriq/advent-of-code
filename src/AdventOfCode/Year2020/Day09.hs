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
import AdventOfCode.TH (evalPuzzle, inputFilePath)
import Control.Foldl qualified as Foldl
import Control.Lens (makeLenses, use, (<.=))
import Data.Bifoldable (bisum)
import Relude
import Text.Trifecta (natural)

data PuzzleState
  = PuzzleState
  { _answerOne :: !(Maybe Int),
    _answerTwo :: !(Maybe Int)
  }
  deriving (Eq, Generic, Show)

makeLenses ''PuzzleState

emptyPuzzleState :: PuzzleState
emptyPuzzleState = PuzzleState Nothing Nothing

main :: IO ()
main = $(evalPuzzle)

getInput :: IO [Int]
getInput = parseInput (some (fromInteger <$> natural)) $(inputFilePath)

partOne :: Puzzle [Int] PuzzleState (Maybe Int)
partOne = do
  numbers <- ask
  answerOne
    <.= (listToMaybe . snd =<< find (null . go) (splitAt 25 <$> tails numbers))
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

partTwo :: Puzzle [Int] PuzzleState (Maybe Int)
partTwo = do
  Just n <- use answerOne
  numbers <- ask
  answerTwo
    <.= ( fmap bisum
            . bisequence
            . Foldl.fold ((,) <$> Foldl.minimum <*> Foldl.maximum)
            =<< find ((n ==) . sum) (concatMap inits (tails numbers))
        )
