{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2024.Day03 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle (Puzzle)
import AdventOfCode.TH (evalPuzzle)
import Control.Lens (makeLenses, (<.=))
import Data.List.Extra (sumOn')
import Relude
import Text.Parser.LookAhead (lookAhead)
import Text.Trifecta (anyChar, comma, decimal, eof, manyTill, parens, string, try)

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

partOne :: Puzzle [Either Bool (Integer, Integer)] PuzzleState Integer
partOne = do
  memory <- asks rights
  answerOne <.= partOne' memory

partOne' :: [(Integer, Integer)] -> Integer
partOne' = sumOn' (uncurry (*))

partTwo :: Puzzle [Either Bool (Integer, Integer)] PuzzleState Integer
partTwo = do
  memory <- ask
  answerTwo <.= partTwo' memory

partTwo' :: [Either Bool (Integer, Integer)] -> Integer
partTwo' = \case
  [] -> 0
  (Left False : muls) -> partTwo' (dropWhile (/= Left True) muls)
  muls ->
    uncurry (+)
      $ bimap (partOne' . rights) partTwo'
      $ span (/= Left False) (dropWhile (== Left False) muls)

getInput :: IO [Either Bool (Integer, Integer)]
getInput = parseInputAoC 2024 3 (some (skipNoise *> instruction))
  where
    instruction = (Left <$> toggle) <|> (Right <$> multiplication)
    skipNoise =
      do
        void
          . manyTill anyChar
          $ lookAhead
            ( void (try multiplication)
                <|> void (try toggle)
                <|> eof
            )
    toggle =
      (True <$ string "do()")
        <|> (False <$ string "don't()")
    multiplication =
      string "mul"
        *> parens ((,) <$> (decimal <* comma) <*> decimal)
