{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2015.Day01 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.SimplePuzzle
import AdventOfCode.TH (evalPuzzle)
import Data.List (elemIndex)
import Relude
import Text.Trifecta (char)

main :: IO ()
main = $(evalPuzzle)

partOne :: SimplePuzzle [Int] Int
partOne = asks sum

partTwo :: SimplePuzzle [Int] (Maybe Int)
partTwo = asks (elemIndex (-1) . scanl' (+) 0)

getInput :: IO [Int]
getInput = parseInputAoC 2015 1 (some instruction)
  where
    instruction = (char '(' $> 1) <|> (char ')' $> -1)
