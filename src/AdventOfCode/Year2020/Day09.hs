{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2020.Day09
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Foldl qualified as Foldl
import Control.Lens (makeLenses, use, (.=), (<.=))
import Data.Bifoldable (bisum)
import Relude
import Text.Trifecta (natural)

data PuzzleState
  = PuzzleState
  { _input :: [Int],
    _answerOne :: Maybe Int,
    _answerTwo :: Maybe Int
  }
  deriving (Eq, Show)

makeLenses ''PuzzleState

newtype Puzzle a
  = Puzzle {runPuzzle :: StateT PuzzleState IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState PuzzleState,
      MonadFail
    )

emptyPuzzleState :: PuzzleState
emptyPuzzleState = PuzzleState [] Nothing Nothing

main :: IO ()
main =
  evaluatingStateT emptyPuzzleState $ runPuzzle do
    numbers <- getInput
    input .= numbers
    putStr "Part One: "
    print =<< partOne
    putStr "Part Two: "
    print =<< partTwo

getInput :: (MonadIO m) => m [Int]
getInput = liftIO $ parseInput (some (fromInteger <$> natural)) $(inputFilePath)

partOne :: Puzzle (Maybe Int)
partOne = do
  numbers <- gets _input
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

partTwo :: Puzzle (Maybe Int)
partTwo = do
  Just n <- use answerOne
  numbers <- gets _input
  answerTwo
    <.= ( fmap bisum
            . bisequence
            . Foldl.fold ((,) <$> Foldl.minimum <*> Foldl.maximum)
            =<< find ((n ==) . sum) (concatMap inits (tails numbers))
        )
