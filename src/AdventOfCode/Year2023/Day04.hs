{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Year2023.Day04 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Arrow ((>>>))
import Control.Monad (void)
import Data.List (intercalate)
import Data.List.Extra (sumOn')
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Trifecta hiding (parseString)

newtype Scratchcard = Scratchcard {unScratchcard :: (Set Int, Set Int)}
  deriving (Eq, Ord, Show)

main :: IO ()
main = $(defaultMain)

partOne :: [Scratchcard] -> Int
partOne =
  sumOn' $
    countWinners >>> \case
      0 -> 0
      k -> 2 ^ (k - 1)

partTwo :: [Scratchcard] -> Int
partTwo = undefined

getInput :: IO [Scratchcard]
getInput = parseInput (some scratchcard) $(inputFilePath)

countWinners :: Scratchcard -> Int
countWinners = Set.size . uncurry Set.intersection . unScratchcard

scratchcard :: Parser Scratchcard
scratchcard =
  do
    void (symbol "Card" *> posInt <* symbol ":")
    winningNumbers <- Set.fromList <$> (some posInt <* symbol "|")
    punterNumbers <- Set.fromList <$> some posInt
    pure (Scratchcard (winningNumbers, punterNumbers))

posInt :: Parser Int
posInt = fromInteger <$> natural

partOneExample :: IO Int
partOneExample = partOne <$> getExample

partTwoExample :: IO Int
partTwoExample = partTwo <$> getExample

getExample :: IO [Scratchcard]
getExample = parseString (some scratchcard) example

example :: String
example =
  intercalate
    "\n"
    [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
      "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
      "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
      "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
      "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
      "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    ]
