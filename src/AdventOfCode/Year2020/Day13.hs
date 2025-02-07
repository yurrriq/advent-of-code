{-# LANGUAGE TypeApplications #-}

module AdventOfCode.Year2020.Day13 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Arrow (second, (&&&))
import Data.Function (on)
import Data.List.Extra (minimumOn)
import Data.Maybe (catMaybes)
import Text.Trifecta (Parser, char, commaSep, decimal, newline)

main :: IO ()
main = $(defaultMain)

partOne :: (Integer, [Maybe Integer]) -> Integer
partOne (timestamp, busIDs) =
  let (soonestBus, (q, _)) =
        minimumOn (snd . snd) $
          (id &&& second (abs . (1 -)) . properFraction @Double . ((/) `on` fromInteger) timestamp)
            <$> catMaybes busIDs

      wait = (soonestBus * (q + 1)) - timestamp
   in soonestBus * wait

partTwo :: (Integer, [Maybe Integer]) -> Integer
partTwo = undefined

getInput :: IO (Integer, [Maybe Integer])
getInput = parseInput notes $(inputFilePath)

notes :: Parser (Integer, [Maybe Integer])
notes =
  do
    (,)
      <$> (decimal <* newline)
      <*> commaSep (Just <$> decimal <|> Nothing <$ char 'x')

getExample :: IO (Integer, [Maybe Integer])
getExample = parseString notes example

example :: String
example =
  "939\n\
  \7,13,x,x,59,x,31,19\n"
