{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2022.Day02 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.Finite (Finite, getFinite)
import Relude
import Text.Trifecta (Parser, char, choice, newline, sepEndBy, space)

type Z3 = Finite 3

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO [(Z3, Z3)]
getInput = parseInputAoC 2022 2 strategyGuide

partOne :: SimplePuzzle [(Z3, Z3)] Integer
partOne = asks (followGuide snd scoreOutcome)
  where
    scoreOutcome (opponent, player) = player + (1 - opponent)

partTwo :: SimplePuzzle [(Z3, Z3)] Integer
partTwo = asks (followGuide scoreShape snd)
  where
    scoreShape (opponent, outcome) = outcome - (1 - opponent)

followGuide :: ((Z3, Z3) -> Z3) -> ((Z3, Z3) -> Z3) -> [(Z3, Z3)] -> Integer
followGuide scoreShape scoreOutcome =
  getSum . foldMap (Sum . followAdvice scoreShape scoreOutcome)

followAdvice :: ((Z3, Z3) -> Z3) -> ((Z3, Z3) -> Z3) -> (Z3, Z3) -> Integer
followAdvice scoreShape scoreOutcome guide =
  1 + getFinite (scoreShape guide) + 3 * getFinite (scoreOutcome guide)

strategyGuide :: Parser [(Z3, Z3)]
strategyGuide = ((,) <$> opponent <*> advice) `sepEndBy` newline
  where
    opponent =
      choice
        [ (char 'A' <* space) $> 0,
          (char 'B' <* space) $> 1,
          (char 'C' <* space) $> 2
        ]
    advice =
      choice
        [ char 'X' $> 0,
          char 'Y' $> 1,
          char 'Z' $> 2
        ]
