module AdventOfCode.Year2021.Day08 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Text.Trifecta

main :: IO ()
main = $(defaultMain)

partOne :: [([String], [String])] -> Int
partOne = foldr ((+) . go) 0
  where
    go = length . filter (`elem` [2, 3, 4, 7]) . map length . snd

partTwo :: [([String], [String])] -> Int
partTwo = undefined

getInput :: IO [([String], [String])]
getInput = parseInput (some display) $(inputFilePath)
  where
    display =
      do
        connections <- count 10 segment
        _ <- symbol "|"
        output <- count 4 segment
        pure (connections, output)
    segment = some (oneOf "abcdefg") <* try space
