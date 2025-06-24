module AdventOfCode.Year2024.Day05 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Arrow ((&&&))
import Control.Monad (void)
import Data.Graph qualified as Graph
import Text.Trifecta hiding (parseString)

main :: IO ()
main = $(defaultMain)

partOne :: ([(Int, Int)], [[Int]]) -> Int
partOne (rules, updates) =
  sum
    [ middle update
      | update <- updates,
        update == applyRules rules update
    ]

partTwo :: ([(Int, Int)], [[Int]]) -> Int
partTwo (rules, updates) =
  sum
    [ middle sorted
      | update <- updates,
        let sorted = applyRules rules update,
        update /= sorted
    ]

-- TODO: look into fgl instead
applyRules :: [(Int, Int)] -> [Int] -> [Int]
applyRules rules update = filter (`elem` update) (Graph.topSort graph)
  where
    graph = Graph.buildG bounds relevantRules
    relevantRules = filter (all (`elem` update)) rules
    bounds =
      minimum &&& maximum $
        foldMap (\(before, after) -> [before, after]) relevantRules

getInput :: IO ([(Int, Int)], [[Int]])
getInput = parseInput safetyManual $(inputFilePath)

safetyManual :: Parser ([(Int, Int)], [[Int]])
safetyManual =
  do
    orderingRules <- orderingRule `sepEndBy` newline
    void newline
    updates <- update `sepEndBy` newline
    pure (orderingRules, updates)
  where
    update = posInt `sepBy1` comma
    posInt = fromInteger <$> decimal
    orderingRule =
      (,)
        <$> (posInt <* char '|')
        <*> posInt

getExample :: IO ([(Int, Int)], [[Int]])
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
