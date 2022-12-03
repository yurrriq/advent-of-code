{-# LANGUAGE DataKinds #-}

module AdventOfCode.Year2022.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.Finite (Finite, getFinite)
import Data.Functor (($>))
import Data.Monoid (Sum (..), getSum)
import Text.Trifecta (Parser, char, choice, newline, sepEndBy, space)

type Z3 = Finite 3

main :: IO ()
main = $(defaultMain)

getInput :: IO [(Z3, Z3)]
getInput = parseInput strategyGuide $(inputFilePath)

partOne :: [(Z3, Z3)] -> Integer
partOne = followGuide snd scoreOutcome
  where
    scoreOutcome (opponent, player) = player + (1 - opponent)

partTwo :: [(Z3, Z3)] -> Integer
partTwo = followGuide scoreShape snd
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
