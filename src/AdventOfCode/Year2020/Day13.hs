module AdventOfCode.Year2020.Day13 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Data.List.Extra (minimumOn)
import Data.Maybe (catMaybes)
import Text.Trifecta (Parser, char, commaSep, decimal, newline)

main :: IO ()
main = $(defaultMain)

partOne :: (Integer, [Maybe Integer]) -> Integer
partOne (timestamp, busIDs) =
  uncurry (*) $
    minimumOn snd $
      [ (busID, wait)
        | busID <- catMaybes busIDs,
          let wait = busID - (timestamp `mod` busID)
      ]

partTwo :: (Integer, [Maybe Integer]) -> Integer
partTwo = undefined

getInput :: IO (Integer, [Maybe Integer])
getInput = parseInput notes $(inputFilePath)

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
