module AdventOfCode.Year2022.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.Functor (($>))
import Data.Monoid (Sum (..))
import Text.Trifecta

data Opponent
  = A
  | B
  | C
  deriving (Eq, Enum, Show)

data Player
  = X
  | Y
  | Z
  deriving (Eq, Enum, Show)

data Choice
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Enum, Show)

data Outcome
  = Loss
  | Draw
  | Win
  deriving (Eq, Enum, Show)

main :: IO ()
main = $(defaultMain)

getInput :: IO [(Opponent, Player)]
getInput = parseInput strategyGuide $(inputFilePath)

partOne :: [(Opponent, Player)] -> Int
partOne = getSum . foldMap (Sum . uncurry score)
  where
    score opponent player = scoreChoice player' + scoreRound opponent' player'
      where
        opponent' = translateOpponent opponent
        player' = translatePlayer player

    translatePlayer X = Rock
    translatePlayer Y = Paper
    translatePlayer Z = Scissors

    scoreRound Scissors Rock = 6
    scoreRound Paper Scissors = 6
    scoreRound Rock Paper = 6
    scoreRound x y
      | x == y = 3
      | otherwise = 0

partTwo :: [(Opponent, Player)] -> Int
partTwo = getSum . foldMap (Sum . uncurry score)
  where
    score opponent outcome = scoreChoice player' + (3 * fromEnum outcome)
      where
        player' = translate (translateOpponent opponent) (translateOutcome outcome)

        translate Rock Loss = Scissors
        translate Rock Win = Paper
        translate Paper Loss = Rock
        translate Paper Win = Scissors
        translate Scissors Loss = Paper
        translate Scissors Win = Rock
        translate choice Draw = choice

        translateOutcome X = Loss
        translateOutcome Y = Draw
        translateOutcome Z = Win

strategyGuide :: Parser [(Opponent, Player)]
strategyGuide = ((,) <$> opponent <*> player) `sepEndBy` newline
  where
    opponent =
      choice
        [ (char 'A' <* space) $> A,
          (char 'B' <* space) $> B,
          (char 'C' <* space) $> C
        ]
    player =
      choice
        [ char 'X' $> X,
          char 'Y' $> Y,
          char 'Z' $> Z
        ]

scoreChoice :: Choice -> Int
scoreChoice = succ . fromEnum

translateOpponent A = Rock
translateOpponent B = Paper
translateOpponent C = Scissors
