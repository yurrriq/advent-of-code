{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2023.Day06 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (bitraverseBoth, (<.>))
import Data.List.Extra (productOn')
import Relude
import Relude.Extra.Bifunctor (bimapBoth)
import Text.Trifecta (Parser, natural, symbol)

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle [(Int, Int)] Int
partOne = asks (productOn' waysToWin)

partTwo :: SimplePuzzle [(Int, Int)] Int
partTwo =
  ask
    >>= waysToWin
    <.> bitraverseBoth (parseString posInt)
    . foldMap (bimapBoth (show @String))

waysToWin :: (Int, Int) -> Int
waysToWin = bimapBoth fromIntegral >>> waysToWin'

waysToWin' :: (Double, Double) -> Int
waysToWin' (time, distance) = maxTime - minTime + 1
  where
    maxTime = floor ((time + delta) / 2)
    minTime = ceiling ((time - delta) / 2)
    delta = sqrt $ time ** 2 - 4 * distance

getInput :: IO [(Int, Int)]
getInput = parseInputAoC 2023 6 document

document :: Parser [(Int, Int)]
document =
  zip
    <$> (symbol "Time:" *> some posInt)
    <*> (symbol "Distance:" *> some posInt)

posInt :: Parser Int
posInt = fromInteger <$> natural

getExample :: IO [(Int, Int)]
getExample = parseString document example

example :: String
example =
  "Time:      7  15   30\n\
  \Distance:  9  40  200\n"
