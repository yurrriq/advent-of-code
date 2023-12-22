module AdventOfCode.Year2023.Day06 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (count)
import Data.List.Extra (productOn')
import Text.Trifecta (Parser, natural, some, symbol)

main :: IO ()
main = $(defaultMain)

partOne :: [(Int, Int)] -> Int
partOne = productOn' waysToWin

partTwo :: [(Int, Int)] -> Int
partTwo races = waysToWin (time, distance)
  where
    time = read $ concatMap (show . fst) races
    distance = read $ concatMap (show . snd) races

waysToWin :: (Int, Int) -> Int
waysToWin (time, distance) =
  count (\speed -> (time - speed) * speed > distance) [1 .. time - 1]

getInput :: IO [(Int, Int)]
getInput = parseInput document $(inputFilePath)

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
