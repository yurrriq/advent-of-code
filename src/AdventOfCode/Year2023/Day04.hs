{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2023.Day04 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.IntMap.Strict ((!))
import Data.IntMap.Strict qualified as IntMap
import Data.List.Extra (sumOn')
import Data.Set qualified as Set
import Relude
import Text.Trifecta hiding (parseString)

newtype Scratchcard = Scratchcard {unScratchcard :: (Int, (Set Int, Set Int))}
  deriving (Eq, Ord, Show)

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle [Scratchcard] Int
partOne =
  asks
    $ sumOn'
    $ countWinners
    >>> \case
      0 -> 0
      k -> 2 ^ (k - 1)

partTwo :: SimplePuzzle [Scratchcard] Int
partTwo = asks \cards ->
  let winCopies copies card@(Scratchcard (i, _)) =
        case countWinners card of
          0 -> copies
          k -> foldl' (flip (IntMap.adjust (+ copies ! i))) copies [i + 1 .. i + k]
      initial = IntMap.fromList [(i, 1) | Scratchcard (i, _) <- cards]
      sortedCards = sortOn (fst . unScratchcard) cards
   in sum . IntMap.elems $ foldl' winCopies initial sortedCards

getInput :: IO [Scratchcard]
getInput = parseInputAoC 2023 4 (some scratchcard)

countWinners :: Scratchcard -> Int
countWinners (Scratchcard (_, (winners, numbers))) =
  Set.size $ winners `Set.intersection` numbers

scratchcard :: Parser Scratchcard
scratchcard =
  do
    i <- symbol "Card" *> posInt <* symbol ":"
    winners <- Set.fromList <$> (some posInt <* symbol "|")
    numbers <- Set.fromList <$> some posInt
    pure (Scratchcard (i, (winners, numbers)))

posInt :: Parser Int
posInt = fromInteger <$> natural

partOneExample :: IO Int
partOneExample = evaluatingPuzzle partOne =<< getExample

partTwoExample :: IO Int
partTwoExample = evaluatingPuzzle partTwo =<< getExample

getExample :: IO [Scratchcard]
getExample = parseString (some scratchcard) example

example :: String
example =
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
  \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
  \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
  \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
  \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
  \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n"
