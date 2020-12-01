module AdventOfCode.Year2020.Day01
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Util (parseInput)
import Data.List (tails)
import Data.Maybe (listToMaybe)
import Text.Trifecta (Parser, natural, some)

main :: IO ()
main =
  do
    entries <- parseInput (some posInt) "input/2020/day01.txt"
    putStr "Part One: "
    print $ partOne entries
    putStr "Part Two: "
    print $ partTwo entries

partOne :: [Int] -> Maybe Int
partOne entries =
  product
    <$> listToMaybe
      [ [x, y]
        | (x : ys) <- tails entries,
          y <- ys,
          x + y == 2020
      ]

partTwo :: [Int] -> Maybe Int
partTwo entries =
  product
    <$> listToMaybe
      [ [x, y, z]
        | (x : ys) <- tails entries,
          (y : zs) <- tails ys,
          z <- zs,
          x + y + z == 2020
      ]

posInt :: Parser Int
posInt = fromInteger <$> natural
