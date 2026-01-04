{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2020.Day13 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.List.Extra (minimumOn)
import Relude
import Text.Trifecta (Parser, char, commaSep, decimal, newline)
import Prelude (until)

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle (Integer, [Maybe Integer]) Integer
partOne =
  ask <&> \(timestamp, busIDs) ->
    uncurry (*)
      $ minimumOn snd
      $ [ (busID, wait)
        | busID <- catMaybes busIDs,
          let wait = busID - (timestamp `mod` busID)
        ]

partTwo :: SimplePuzzle (Integer, [Maybe Integer]) Integer
partTwo =
  asks snd
    <&> fst
    . foldl' go (0, 1)
    . catMaybes
    . zipWith (fmap . (,)) [0 ..]
  where
    go (base, step) (offset, busID) =
      ( until ((== 0) . (`mod` busID) . (+ offset)) (+ step) base,
        step * busID
      )

getInput :: IO (Integer, [Maybe Integer])
getInput = parseInputAoC 2020 13 notes

notes :: Parser (Integer, [Maybe Integer])
notes =
  (,)
    <$> (decimal <* newline)
    <*> commaSep (Just <$> decimal <|> Nothing <$ char 'x')

getExample :: IO (Integer, [Maybe Integer])
getExample = parseString notes example

example :: String
example =
  "939\n\
  \7,13,x,x,59,x,31,19\n"
