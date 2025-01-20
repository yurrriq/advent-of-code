{-# LANGUAGE MonadComprehensions #-}

module AdventOfCode.Year2024.Day07 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (numDigits)
import Data.Foldable (foldrM)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Text.Trifecta (Parser, char, decimal, newline, sepEndBy, sepEndByNonEmpty, string)

main :: IO ()
main = $(defaultMain)

partOne :: [(Int, NonEmpty Int)] -> Int
partOne = calibrate [unAdd, unMultiply]

partTwo :: [(Int, NonEmpty Int)] -> Int
partTwo = calibrate [unAdd, unMultiply, unConcatenate]

-- | Given a list of operators and a list of calibration equations, compute the
-- calibration result, i.e., the sum of the test values from just the equations
-- that could possibly be true.
calibrate :: [Int -> Int -> Maybe Int] -> [(Int, NonEmpty Int)] -> Int
calibrate operators = foldl' go 0
  where
    go acc eq =
      if isPossible operators eq
        then fst eq + acc
        else acc

-- | Given a list of operators, determine if a given calibration equation is
-- possible, i.e., placing some combination of operators into the equation
-- produces the test value.
--
-- Process the operands from right to left, using the inverses of the given
-- operators, to short circuit on operations that make the equation impossible.
--
-- >>> isPossible [unAdd, unMultiply] (190, 10 :| [19])
-- True
isPossible :: [Int -> Int -> Maybe Int] -> (Int, NonEmpty Int) -> Bool
isPossible operators (testValue, operand :| operands) =
  operand `elem` foldrM go testValue operands
  where
    go x y = mapMaybe (\f -> f x y) operators

-- | The inverse of addition.
--
-- >>> unAdd 40 121
-- Just 81
unAdd :: Int -> Int -> Maybe Int
unAdd x y = [y - x | y >= x]

-- | The inverse of multiplication.
--
-- >>> unMultiply 19 190
-- Just 10
unMultiply :: Int -> Int -> Maybe Int
unMultiply x y = [y `div` x | y `mod` x == 0]

-- | The inverse of concatenation.
--
-- >>> unConcatenate 345 12345
-- Just 12
unConcatenate :: Int -> Int -> Maybe Int
unConcatenate x y = [d | m == x]
  where
    pow = numDigits x
    (d, m) = y `divMod` (10 ^ pow)

getInput :: IO [(Int, NonEmpty Int)]
getInput = parseInput (calibrationEquation `sepEndBy` newline) $(inputFilePath)

-- | Parse a calibration equation, i.e., a test value and a nonempty list of
-- operands.
calibrationEquation :: Parser (Int, NonEmpty Int)
calibrationEquation =
  (,)
    <$> (decimalInt <* string ": ")
    <*> decimalInt `sepEndByNonEmpty` char ' '
  where
    decimalInt = fromInteger <$> decimal

getExample :: IO [(Int, NonEmpty Int)]
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
