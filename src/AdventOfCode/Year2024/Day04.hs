{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2024.Day04 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (count)
import Control.Lens ((<.=))
import Data.List.Extra (dropEnd, sumOn')
import Data.Universe.Helpers (diagonals)
import Linear (V2 (..))
import Relude
import Text.Trifecta (Parser, manyTill, newline, oneOf)

type PuzzleState = GPuzzleState1 Int

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: Puzzle [[Char]] PuzzleState Int
partOne = do
  grid <- ask
  let gridRotatedRight90 = transpose (reverse grid)
  answerOne
    <.= sum
      [ countHorizontal grid,
        countHorizontal gridRotatedRight90,
        countDiagonal grid,
        countDiagonal gridRotatedRight90
      ]
  where
    countHorizontal =
      sumOn' (count ((`elem` ["XMAS", "SAMX"]) . take 4) . dropEnd 4 . tails)
    countDiagonal =
      countHorizontal . dropEnd 3 . drop 3 . diagonals

partTwo :: Puzzle [[Char]] PuzzleState Int
partTwo = do
  grid <- ask
  let size = length grid
  answerTwo
    <.= count
      ((`elem` ["MMSS", "MSMS", "SMSM", "SSMM"]) . mapMaybe (square grid) . corners)
      [ V2 x y
      | x <- [0 .. size - 3],
        y <- [0 .. size - 3],
        square grid (V2 x y + 1) == Just 'A'
      ]

corners :: (Num a) => V2 a -> [V2 a]
corners xy = [xy + V2 a b | a <- [0, 2], b <- [0, 2]]

square :: [[Char]] -> V2 Int -> Maybe Char
square grid (V2 x y) = grid !!? y >>= maybeAt x

getInput :: IO [[Char]]
getInput = parseInputAoC 2024 4 wordSearch

wordSearch :: Parser [[Char]]
wordSearch = some (manyTill (oneOf "XMAS") newline)

getExample :: IO [[Char]]
getExample =
  parseString
    wordSearch
    "MMMSXXMASM\n\
    \MSAMXMSMSA\n\
    \AMXSXMAAMM\n\
    \MSAMASMSMX\n\
    \XMASAMXAMM\n\
    \XXAMMXXAMA\n\
    \SMSMSASXSS\n\
    \SAXAMASAAA\n\
    \MAMMMXMMMM\n\
    \MXMXAXMASX\n"
