module AdventOfCode.Year2022.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.Functor (($>))
import Data.Monoid (Sum (..))
import Text.Trifecta

data Choice
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Enum, Show)

main :: IO ()
main = $(defaultMain)

getInput :: IO [(Choice, Choice)]
getInput = parseInput strategyGuide $(inputFilePath)

partOne :: [(Choice, Choice)] -> Int
partOne = getSum . foldMap (Sum . uncurry score)

partTwo :: [(Choice, Choice)] -> Int
partTwo = undefined

strategyGuide :: Parser [(Choice, Choice)]
strategyGuide = ((,) <$> opponent <*> player) `sepEndBy` newline
  where
    opponent =
      choice
        [ (char 'A' <* space) $> Rock,
          (char 'B' <* space) $> Paper,
          (char 'C' <* space) $> Scissors
        ]
    player =
      choice
        [ char 'X' $> Rock,
          char 'Y' $> Paper,
          char 'Z' $> Scissors
        ]

score :: Choice -> Choice -> Int
score opponent player = scoreChoice player + scoreRound opponent player

scoreRound :: Choice -> Choice -> Int
scoreRound Scissors Rock = 6
scoreRound Paper Scissors = 6
scoreRound Rock Paper = 6
scoreRound x y
  | x == y = 3
  | otherwise = 0

scoreChoice :: Choice -> Int
scoreChoice = succ . fromEnum
