module AdventOfCode.Year2024.Day04 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (count)
import Data.List (tails, transpose)
import Data.List.Extra (dropEnd, sumOn')
import Data.Universe.Helpers (diagonals)
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
  sum
    [ 1
      | x <- [0 .. width - 3],
        y <- [0 .. height - 3],
        (grid !! (y + 1) !! (x + 1) == 'A')
          && ( ( grid !! y !! x == 'M'
                   && grid !! y !! (x + 2) == 'M'
                   && grid !! (y + 2) !! x == 'S'
                   && grid !! (y + 2) !! (x + 2) == 'S'
               )
                 || ( grid !! y !! x == 'M'
                        && grid !! y !! (x + 2) == 'S'
                        && grid !! (y + 2) !! x == 'M'
                        && grid !! (y + 2) !! (x + 2) == 'S'
                    )
                 || ( grid !! y !! x == 'S'
                        && grid !! y !! (x + 2) == 'S'
                        && grid !! (y + 2) !! x == 'M'
                        && grid !! (y + 2) !! (x + 2) == 'M'
                    )
                 || ( grid !! y !! x == 'S'
                        && grid !! y !! (x + 2) == 'M'
                        && grid !! (y + 2) !! x == 'S'
                        && grid !! (y + 2) !! (x + 2) == 'M'
                    )
             )
    ]
  where
    width = length (head grid)
    height = length grid

getInput :: IO [[Char]]
getInput =
  parseInput wordSearch $(inputFilePath)

wordSearch :: Parser [[Char]]
wordSearch = some (manyTill xmas newline)
  where
    xmas = oneOf "XMAS"

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
