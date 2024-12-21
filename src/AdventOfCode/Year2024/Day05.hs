module AdventOfCode.Year2024.Day05 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Monad (void)
import Data.List.Extra (sumOn')
import Data.Maybe (fromMaybe)
import Text.Trifecta hiding (parseString)

main :: IO ()
main = $(defaultMain)

partOne :: ([(Integer, Integer)], [[Integer]]) -> Integer
partOne (rules, updates) = sumOn' middle (filter isOrdered updates)
  where
    isOrdered update =
      let positions = zip update [0 :: Int ..]
       in and
            [ fromMaybe True $
                liftA2 (<) (lookup before positions) (lookup after positions)
              | (before, after) <- rules
            ]

partTwo :: ([(Integer, Integer)], [[Integer]]) -> Integer
partTwo = undefined

getInput :: IO ([(Integer, Integer)], [[Integer]])
getInput = parseInput safetyManual $(inputFilePath)

safetyManual :: Parser ([(Integer, Integer)], [[Integer]])
safetyManual =
  do
    orderingRules <- orderingRule `sepEndBy` newline
    void newline
    updates <- update `sepEndBy` newline
    pure (orderingRules, updates)
  where
    update = decimal `sepBy1` comma
    orderingRule =
      (,)
        <$> (decimal <* char '|')
        <*> decimal

getExample :: IO ([(Integer, Integer)], [[Integer]])
getExample = parseString safetyManual example

example :: String
example =
  "47|53\n\
  \97|13\n\
  \97|61\n\
  \97|47\n\
  \75|29\n\
  \61|13\n\
  \75|53\n\
  \29|13\n\
  \97|29\n\
  \53|29\n\
  \61|53\n\
  \97|53\n\
  \61|29\n\
  \47|13\n\
  \75|47\n\
  \97|75\n\
  \47|61\n\
  \75|61\n\
  \47|29\n\
  \75|13\n\
  \53|13\n\
  \\n\
  \75,47,61,53,29\n\
  \97,61,53,29,13\n\
  \75,29,13\n\
  \75,97,47,61,53\n\
  \61,13,29\n\
  \97,13,75,29,47\n"

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)
