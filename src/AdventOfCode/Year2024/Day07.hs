{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeApplications #-}

module AdventOfCode.Year2024.Day07 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Combinatorics (variateRep)
import Data.Function (on)
import Data.Function.Pointless ((.:))
import Data.List (uncons)
import Text.Trifecta (Parser, char, decimal, newline, sepEndBy, string)
import Prelude hiding ((||))

main :: IO ()
main = $(defaultMain)

partOne :: [(Integer, [Integer])] -> Integer
partOne = calibrate [(+), (*)]

numDigits :: Integer -> Int
numDigits n = fromIntegral @Integer (floor (logBase 10 (fromInteger @Double n)) + 1)

partTwo :: [(Integer, [Integer])] -> Integer
partTwo = calibrate [(+), (*), (||)]
  where
    (||) = read .: (++) `on` show

calibrate :: (Foldable t, Ord a, Num a) => [a -> a -> a] -> t (a, [a]) -> a
calibrate operators = foldl go 0
  where
    go acc eq =
      if isPossible operators eq
        then fst eq + acc
        else acc

isPossible :: (Ord a) => [a -> a -> a] -> (a, [a]) -> Bool
isPossible operators (testValue, operands) =
  any (go operands) operatorLists
  where
    operatorLists = variateRep (length operands - 1) operators
    go [x] []
      | testValue == x = True
      | otherwise = False
    go (x : operands') (op : ops) =
      (x <= testValue)
        && maybe False (\(y, ys) -> go (x `op` y : ys) ops) (uncons operands')
    go _ _ = False

getInput :: IO [(Integer, [Integer])]
getInput = parseInput (calibrationEquation `sepEndBy` newline) $(inputFilePath)

calibrationEquation :: Parser (Integer, [Integer])
calibrationEquation =
  (,)
    <$> (decimal <* string ": ")
    <*> (decimal `sepEndBy` char ' ')

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
