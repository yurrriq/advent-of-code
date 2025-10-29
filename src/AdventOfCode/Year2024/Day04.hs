module AdventOfCode.Year2024.Day04 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (count)
import Data.List (tails, transpose)
import Data.List.Extra (dropEnd, sumOn')
import Data.Universe.Helpers (diagonals)
import Linear (V2 (..))
import Text.Trifecta (Parser, manyTill, newline, oneOf, some)

main :: IO ()
main = $(defaultMain)

partOne :: [[Char]] -> Int
partOne grid =
  sum
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
    gridRotatedRight90 = transpose (reverse grid)

partTwo :: [[Char]] -> Int
partTwo grid =
  count
    ((`elem` ["MMSS", "MSMS", "SMSM", "SSMM"]) . map (square grid) . corners)
    [ V2 x y
    | x <- [0 .. size - 3],
      y <- [0 .. size - 3],
      square grid (V2 x y + 1) == 'A'
    ]
  where
    size = length grid

corners :: (Num a) => V2 a -> [V2 a]
corners xy = [xy + V2 a b | a <- [0, 2], b <- [0, 2]]

square :: [[Char]] -> V2 Int -> Char
square grid (V2 x y) = grid !! y !! x

getInput :: IO [[Char]]
getInput = parseInput wordSearch $(inputFilePath)

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
