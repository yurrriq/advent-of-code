module AdventOfCode.Year2020.Day06
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Data.Set qualified as Set
import Text.Trifecta (Parser, lower, newline, sepEndBy, some)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne input
    putStr "Part Two: "
    print $ partTwo input

getInput :: IO [[String]]
getInput = parseInput (group `sepEndBy` newline) $(inputFilePath)

partOne :: [[String]] -> Int
partOne = sum . map (Set.size . foldr go Set.empty)
  where
    go = flip (foldr Set.insert)

partTwo :: [[String]] -> Int
partTwo =
  sum . map (Set.size . foldr1 Set.intersection . map Set.fromList)

group :: Parser [String]
group = questionnaire `sepEndBy` newline

questionnaire :: Parser String
questionnaire = some lower
