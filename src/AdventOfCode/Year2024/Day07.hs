{-# LANGUAGE MonadComprehensions #-}

-- |
-- Module      : AdventOfCode.Year2024.Day07
-- Description : Advent of Code 2024 Day 7: Bridge Repair
-- Copyright   : (c) Eric Bailey, 2025
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
-- https://adventofcode.com/2024/day/7
module AdventOfCode.Year2024.Day07
  ( -- * Introduction
    -- $intro

    -- * Data types
    CalibrationEquation,
    Operator (..),

    -- * Solution
    main,
    partOne,
    partTwo,
    execInverseOperation,
    calibrate,
    isPossible,

    -- * Input and example
    getInput,
    getExample,
  )
where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (numDigits)
import Data.Foldable (foldrM)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)
import Text.Trifecta (Parser, char, decimal, newline, sepEndBy, sepEndByNonEmpty, string)

-- $intro
-- This module borrows
-- [ideas](https://github.com/mstksg/advent-of-code/wiki/Reflections-2024#day-7)
-- from [Justin Le](https://blog.jle.im). In particular, 'isPossible' is
-- essentially the same as Justin's
-- [@search@](https://github.com/mstksg/advent-of-code/blob/0128ceaf/2024/AOC2024/Day07.hs#L24-L27).
-- The main idea is to process the operands from right to left, which enables
-- quickly filtering out equations that are impossible.

-- | A calibration equation consists of a test value and a nonempty list of
-- operands.
type CalibrationEquation = (Int, NonEmpty Int)

-- | An invertible binary operator.
data Operator
  = -- | Add
    (:+:)
  | -- | Multiply
    (:*:)
  | -- | Concatenate
    (:||:)
  deriving (Eq, Show, Enum, Bounded)

-- | Given an operator and two operands, execute the inverse operation.
--
-- >>> execInverseOperation (:+:) 40 (81 + 40)
-- Just 81
--
-- >>> execInverseOperation (:*:) 19 (10 * 19)
-- Just 10
--
-- >>> execInverseOperation (:||:) 345 12345
-- Just 12
execInverseOperation :: Operator -> Int -> Int -> Maybe Int
execInverseOperation operator x y =
  case operator of
    (:+:) -> [y - x | y >= x]
    (:*:) -> [y `div` x | y `mod` x == 0]
    (:||:) ->
      let pow = numDigits x
          (d, m) = y `divMod` (10 ^ pow)
       in [d | m == x]

-- | Solve the puzzle and print the results.
main :: IO ()
main = $(defaultMain)

-- | Calibrate the equations using addition and multiplication.
--
-- @partOne = 'calibrate' ['(:+:)', '(:*:)']@
partOne :: [CalibrationEquation] -> Int
partOne = calibrate [(:+:), (:*:)]

-- | Calibrate the equations using addition, multiplication, and concatenation.
--
-- @partTwo = 'calibrate' ['(:+:)', '(:*:)', '(:||:)']@
partTwo :: [CalibrationEquation] -> Int
partTwo = calibrate [(:+:), (:*:), (:||:)]

-- | Given a list of operators and a list of calibration equations, compute the
-- sum of the test values from just the equations that could possibly be true.
--
-- >>> calibrate [(:+:), (:*:)] <$> getExample
-- 3749
--
-- >>> calibrate [(:+:), (:*:), (:||:)] <$> getExample
-- 11387
calibrate :: [Operator] -> [CalibrationEquation] -> Int
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
-- @190: 10 19@ has only one position that accepts an operator: between @10@ and
-- @19@. Choosing @+@ would give @29@, but choosing @*@ would give the test
-- value (@10 * 19 = 190@).
--
-- >>> isPossible [(:+:), (:*:)] (190, 10 :| [19])
-- True
--
-- @3267: 81 40 27@ has two positions for operators. Of the four possible
-- configurations of the operators, /two/ cause the right side to match the test
-- value: @81 + 40 * 27@ and @81 * 40 + 27@ both equal @3267@ (when evaluated
-- left-to-right)!
--
-- >>> isPossible [(:+:), (:*:)] (3267, 81 :| [40, 27])
-- True
--
-- @292: 11 6 16 20@ can be solved in exactly one way: @11 + 6 * 16 + 20@.
--
-- >>> isPossible [(:+:), (:*:)] (292, 11 :| [6, 16, 20])
-- True
isPossible :: [Operator] -> CalibrationEquation -> Bool
isPossible operators (testValue, operand :| operands) =
  operand `elem` foldrM go testValue operands
  where
    go x y = mapMaybe (\operator -> execInverseOperation operator x y) operators

-- | Parse the input into a list of calibration equations.
getInput :: IO [CalibrationEquation]
getInput = parseInput (calibrationEquation `sepEndBy` newline) $(inputFilePath)

-- | Parse a calibration equation.
calibrationEquation :: Parser CalibrationEquation
calibrationEquation =
  (,)
    <$> (decimalInt <* string ": ")
    <*> decimalInt `sepEndByNonEmpty` char ' '
  where
    decimalInt = fromInteger <$> decimal

-- | Parse the example into a list of calibration equations.
getExample :: IO [CalibrationEquation]
getExample = parseString (calibrationEquation `sepEndBy` newline) example

-- | The example.
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
