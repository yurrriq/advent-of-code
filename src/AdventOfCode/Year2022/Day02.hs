module AdventOfCode.Year2022.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.Functor (($>))
import Data.Monoid (Sum (..))
import Text.Trifecta (Parser, char, choice, newline, sepEndBy, space)

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
partOne = followGuide score
  where
    score opponent player = scoreChoice player' + scoreOutcome outcome
      where
        opponent' = coerceEnum opponent
        player' = coerceEnum player
        outcome = decideRound opponent' player'

partTwo :: [(Opponent, Player)] -> Int
partTwo = followGuide score
  where
    score opponent outcome = scoreChoice player + scoreOutcome outcome'
      where
        player = translate (coerceEnum opponent) outcome'
        outcome' = coerceEnum outcome

    translate Rock Loss = Scissors
    translate Rock Win = Paper
    translate Paper Loss = Rock
    translate Paper Win = Scissors
    translate Scissors Loss = Paper
    translate Scissors Win = Rock
    translate x Draw = x

followGuide :: (Opponent -> Player -> Int) -> [(Opponent, Player)] -> Int
followGuide score = getSum . foldMap (Sum . uncurry score)

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

decideRound :: Choice -> Choice -> Outcome
decideRound Rock Paper = Win
decideRound Paper Scissors = Win
decideRound Scissors Rock = Win
decideRound opponent player
  | opponent == player = Draw
  | otherwise = Loss

scoreChoice :: Choice -> Int
scoreChoice = succ . fromEnum

scoreOutcome :: Outcome -> Int
scoreOutcome = (3 *) . fromEnum

coerceEnum :: (Enum a, Enum b) => a -> b
coerceEnum = toEnum . fromEnum
