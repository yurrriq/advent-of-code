{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StrictData #-}

module AdventOfCode.Year2020.Day09
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Lens (makeLenses, use, (.=), (<.=))
import Control.Monad (liftM2)
import Control.Monad.State (StateT, evalStateT, gets, liftIO)
import Data.List (find, inits, tails)
import Data.Maybe (listToMaybe)
import Text.Trifecta (natural, some)

data PuzzleState
  = PuzzleState
  { _input :: [Int],
    _answerOne :: Maybe Int,
    _answerTwo :: Maybe Int
  }
  deriving (Eq, Show)

makeLenses ''PuzzleState

type Puzzle = StateT PuzzleState IO

main :: IO ()
main =
  do
    flip evalStateT (PuzzleState [] Nothing Nothing) do
      numbers <- liftIO getInput
      input .= numbers
      liftIO $ putStr "Part One: "
      liftIO . print =<< maybeFail =<< partOne
      liftIO $ putStr "Part Two: "
      liftIO . print =<< maybeFail =<< partTwo

getInput :: IO [Int]
getInput = parseInput (some (fromInteger <$> natural)) $(inputFilePath)

partOne :: Puzzle (Maybe Int)
partOne = do
  numbers <- gets _input
  answerOne <.= (listToMaybe . snd =<< find (null . go) (splitAt 25 <$> tails numbers))
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
    <.= ( liftM2 (+) minimum maximum
            <$> find ((n ==) . sum) (concatMap inits (tails numbers))
        )

maybeFail :: (MonadFail m) => Maybe a -> m a
maybeFail = maybe (fail "failed!") pure
