module AdventOfCode.Year2020.Day05 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Applicative ((<|>))
import Data.FastDigits (undigits)
import Data.List (delete)
import Text.Parser.Token.Highlight (Highlight (..))
import Text.Trifecta (Parser, char, count, highlight, newline, sepEndBy, (<?>))

main :: IO ()
main =
  do
    boardingPasses <- getInput
    putStr "Part One: "
    print $ partOne boardingPasses
    putStr "Part Two: "
    print $ partTwo boardingPasses

getInput :: IO [[Int]]
getInput = parseInput (boardingPass `sepEndBy` newline) $(inputFilePath)

partOne :: [[Int]] -> Integer
partOne = maximum . map findSeatId

-- TODO: in a single pass, subtract sum from sum [min..max]
partTwo :: [[Int]] -> Integer
partTwo bps = head $ foldr delete [firstSeatId .. lastSeatId] seatIds
  where
    -- sumThrough n = n * (n + 1) `div` 2
    firstSeatId = minimum seatIds
    lastSeatId = maximum seatIds
    seatIds = map findSeatId bps

findSeatId :: [Int] -> Integer
findSeatId = undigits (2 :: Int) . reverse

boardingPass :: Parser [Int]
boardingPass = count 10 bsp

bsp :: Parser Int
bsp =
  highlight ReservedIdentifier $
    (char 'F' *> pure 0 <?> "front")
      <|> (char 'L' *> pure 0 <?> "left")
      <|> (char 'B' *> pure 1 <?> "back")
      <|> (char 'R' *> pure 1 <?> "right")
