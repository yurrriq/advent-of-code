{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2020.Day15
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import Control.Lens (makeLenses, use, uses, (%=), (+=), (.=), (<~))
import Control.Monad.Logger (logDebug)
import Data.IntMap qualified as IntMap
import Relude
import Text.Printf (printf)
import Text.Trifecta (commaSep, natural)

data PuzzleState
  = PuzzleState
  { _numbersToSay :: NonEmpty Int,
    _turn :: Int,
    _spokenNumbers :: IntMap Int
  }
  deriving (Eq, Generic, Show)

makeLenses ''PuzzleState

mkState :: NonEmpty Int -> PuzzleState
mkState input = PuzzleState input 0 IntMap.empty

main :: IO ()
main =
  getInput <&> (id &&& mkState) >>= \(input, initialState) ->
    evalPuzzle input initialState do
      putStr "Part One: "
      print =<< partOne
      putStr "Part Two: "
      print =<< partTwo

getInput :: IO (NonEmpty Int)
getInput = fromList <$> parseInputAoC 2020 15 (commaSep posInt)
  where
    posInt = fromInteger <$> natural

partOne :: Puzzle (NonEmpty Int) PuzzleState Int
partOne = memoryGame 2020

partTwo :: Puzzle (NonEmpty Int) PuzzleState Int
partTwo = memoryGame 30_000_000

memoryGame :: Int -> Puzzle (NonEmpty Int) PuzzleState Int
memoryGame n = resetState *> loop
  where
    resetState = put . mkState =<< ask
    loop = do
      turn += 1
      number <- withPuzzle (const (max 1 (n `div` 100))) memoryRound
      uses turn (== n) >>= bool loop (pure number)

memoryRound :: Puzzle Int PuzzleState Int
memoryRound = do
  number :| numbers <- use numbersToSay
  uses spokenNumbers (IntMap.lookup number) >>= \case
    Just before ->
      numbersToSay <~ uses turn (subtract before >>> (:| numbers))
    Nothing ->
      numbersToSay .= fromMaybe (0 :| []) (nonEmpty numbers)
  say number

say :: Int -> Puzzle Int PuzzleState Int
say number =
  use turn >>= \i -> do
    spokenNumbers %= IntMap.insert number i
    m <- ask
    when (i `mod` m == 0 || i `div` m >= 99)
      $ $(logDebug) (fromString (printf "%d: %d" i number))
    pure number
