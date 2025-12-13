{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day10 where

import AdventOfCode.Input (parseInputAoC, parseString)
import Control.Lens (ifoldl')
import Data.Bits (clearBit, setBit)
import Data.List.NonEmpty qualified as NE
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Relude
import Text.Trifecta (Parser, braces, brackets, char, commaSep, natural, parens)

data Machine
  = Machine
  { _lightDiagram :: Int,
    _wiringSchematics :: NonEmpty [Int],
    _joltageRequirements :: Vector Int
  }
  deriving (Eq, Generic, Show)

machine :: Parser Machine
machine = Machine <$> diagram <*> schematics <*> joltages
  where
    diagram =
      ifoldl' (\i acc b -> bool clearBit setBit b acc i) 0
        . reverse
        <$> brackets (some (False <$ char '.' <|> char '#' $> True))
    schematics = NE.some1 (parens (commaSep int))
    joltages = Vector.fromList <$> braces (commaSep int)
    int = fromInteger <$> natural

getExample :: IO [Machine]
getExample = parseString (some machine) example

example :: String
example =
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n\
  \[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n\
  \[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

getInput :: IO [Machine]
getInput = parseInputAoC 2025 10 (some machine)

main :: IO ()
main = fail "not yet implemented"
