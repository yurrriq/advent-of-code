module AdventOfCode.Year2024.Day07 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Combinatorics (variateRep)
import Data.List.Extra (sumOn')
import Data.Maybe (mapMaybe)
import Text.Trifecta (Parser, char, decimal, newline, sepEndBy, string)
import Prelude hiding ((||))

main :: IO ()
main = $(defaultMain)

partOne :: [(Integer, [Integer])] -> Integer
partOne = sumOn' (fst . head) . filter (not . null) . map (insertOperators [(+), (*)])

partTwo :: [(Integer, [Integer])] -> Integer
partTwo = sumOn' (fst . head) . filter (not . null) . map (insertOperators [(+), (*), (||)])
  where
    x || y = read (show x ++ show y)

getInput :: IO [(Integer, [Integer])]
getInput = parseInput (calibrationEquation `sepEndBy` newline) $(inputFilePath)

calibrationEquation :: Parser (Integer, [Integer])
calibrationEquation =
  (,)
    <$> (decimal <* string ": ")
    <*> (decimal `sepEndBy` char ' ')

insertOperators ::
  [Integer -> Integer -> Integer] ->
  (Integer, [Integer]) ->
  [(Integer, [Integer])]
insertOperators operators (testValue, operands) =
  mapMaybe (go operands operands) operatorLists
  where
    operatorLists = variateRep (length operands - 1) operators
    go xs [x] []
      | testValue == x = Just (x, xs)
      | otherwise = Nothing
    go xs (x : y : ys) (op : ops) =
      go xs (x `op` y : ys) ops
    go _ _ _ = undefined

getExample :: IO [(Integer, [Integer])]
getExample = parseString (calibrationEquation `sepEndBy` newline) example

example :: String
example =
  "190: 10 19\n\
  \3267: 81 40 27\n\
  \83: 17 5\n\
  \156: 15 6\n\
  \7290: 6 8 6 15\n\
  \161011: 16 10 13\n\
  \192: 17 8 14\n\
  \21037: 9 7 18 13\n\
  \292: 11 6 16 20\n"
