{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2022.Day05 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.IntMap ((!))
import Data.IntMap qualified as IM
import Relude
import Text.Trifecta
  ( Parser,
    char,
    count,
    decimal,
    newline,
    sepBy,
    surroundedBy,
    symbol,
    token,
    try,
    upper,
  )

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle (IntMap [Char], [(Int, (Int, Int))]) String
partOne = asks (solveWith reverse)

partTwo :: SimplePuzzle (IntMap [Char], [(Int, (Int, Int))]) String
partTwo = asks (solveWith id)

solveWith :: ([Char] -> [Char]) -> (IntMap [Char], [(Int, (Int, Int))]) -> String
solveWith f = mapMaybe listToMaybe . IM.elems . uncurry (rearrange f)

getInput :: IO (IntMap [Char], [(Int, (Int, Int))])
getInput = parseInputAoC 2022 5 drawing

drawing :: Parser (IntMap [Char], [(Int, (Int, Int))])
drawing = (,) <$> (stacks <* labels <* newline) <*> some rearrangement
  where
    labels = void (label `sepBy` char ' ' <* newline)
    label = decimal `surroundedBy` char ' '
    stacks =
      IM.fromList
        . zip [1 ..]
        . map catMaybes
        . transpose
        <$> some ((try crate `sepBy` char ' ') <* newline)
    crate =
      (Just <$> (char '[' *> upper <* char ']'))
        <|> (Nothing <$ count 3 (char ' '))

rearrangement :: Parser (Int, (Int, Int))
rearrangement =
  do
    howMany <- symbol "move" *> posInt
    from <- symbol "from" *> posInt
    to <- symbol "to" *> posInt
    pure (howMany, (from, to))

posInt :: Parser Int
posInt = token (fromInteger <$> decimal)

rearrange :: ([Char] -> [Char]) -> IntMap [Char] -> [(Int, (Int, Int))] -> IntMap [Char]
rearrange _ stacks [] = stacks
rearrange f stacks ((howMany, (from, to)) : rearrangements) =
  rearrange f (IM.insert from ys (IM.adjust (f xs ++) to stacks)) rearrangements
  where
    (xs, ys) = splitAt howMany (stacks ! from)

getExample :: IO (IntMap [Char], [(Int, (Int, Int))])
getExample = parseString drawing example

example :: String
example =
  "    [D]    \n\
  \[N] [C]    \n\
  \[Z] [M] [P]\n\
  \ 1   2   3 \n\
  \\n\
  \move 1 from 2 to 1\n\
  \move 3 from 1 to 3\n\
  \move 2 from 2 to 1\n\
  \move 1 from 1 to 2\n"
