module AdventOfCode.Year2020.Day06
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import qualified Data.HashSet as HashSet
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
partOne = sum . map (HashSet.size . foldr go HashSet.empty)
  where
    go = flip (foldr HashSet.insert)

partTwo :: [[String]] -> Int
partTwo =
  sum . map (HashSet.size . foldr1 HashSet.intersection . map HashSet.fromList)

group :: Parser [String]
group = questionnaire `sepEndBy` newline

questionnaire :: Parser String
questionnaire = some lower
