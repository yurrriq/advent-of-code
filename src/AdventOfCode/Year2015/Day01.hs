module AdventOfCode.Year2015.Day01 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.List (elemIndex, scanl')
import Text.Trifecta (char, some)

main :: IO ()
main = $(defaultMain)

partOne :: [Int] -> Int
partOne = sum

partTwo :: [Int] -> Maybe Int
partTwo = elemIndex (-1) . scanl' (+) 0

getInput :: IO [Int]
getInput = parseInput (some instruction) $(inputFilePath)
  where
    instruction = (char '(' $> 1) <|> (char ')' $> -1)
