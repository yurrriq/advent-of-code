module AdventOfCode.Year2020.Day05 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.Foldl qualified as Foldl
import Control.Monad (guard)
import Data.FastDigits (undigits)
import Text.Parser.Token.Highlight (Highlight (..))
-- TODO: use NonEmpty via sepEndBy1
-- import Control.Applicative.Combinators.NonEmpty  (sepEndBy1)
import Text.Trifecta (Parser, char, count, highlight, newline, sepEndBy, (<?>))

main :: IO ()
main = $(defaultMain)

getInput :: IO [[Int]]
getInput = parseInput (boardingPass `sepEndBy` newline) $(inputFilePath)

partOne :: [[Int]] -> Maybe Integer
partOne = Foldl.fold $ Foldl.premap findSeatId Foldl.maximum

partTwo :: [[Int]] -> Maybe Integer
partTwo =
  Foldl.fold foldSeats >>> \(numSeats, sumIds, maybeMinId, maybeMaxId) -> do
    minId <- maybeMinId
    maxId <- maybeMaxId
    let expectedCount = maxId - minId + 1
    guard (numSeats /= expectedCount)
    pure $ (maxId + minId) * expectedCount `div` 2 - sumIds
  where
    foldSeats =
      Foldl.premap findSeatId $
        (,,,)
          <$> Foldl.genericLength
          <*> Foldl.sum
          <*> Foldl.minimum
          <*> Foldl.maximum

findSeatId :: [Int] -> Integer
findSeatId = undigits @Int 2 . reverse

boardingPass :: Parser [Int]
boardingPass = count 10 bsp

bsp :: Parser Int
bsp =
  highlight ReservedIdentifier $
    (char 'F' *> pure 0 <?> "front")
      <|> (char 'L' *> pure 0 <?> "left")
      <|> (char 'B' *> pure 1 <?> "back")
      <|> (char 'R' *> pure 1 <?> "right")
