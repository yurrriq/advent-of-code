module AdventOfCode.Year2022.Day05 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IM
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Text.Trifecta
  ( Parser,
    char,
    count,
    decimal,
    newline,
    sepBy,
    some,
    surroundedBy,
    symbol,
    token,
    try,
    upper,
  )

main :: IO ()
main = $(defaultMain)

partOne :: (IntMap [Char], [(Int, (Int, Int))]) -> String
partOne = map head . IM.elems . uncurry rearrange

partTwo :: (IntMap [Char], [(Int, (Int, Int))]) -> String
partTwo = undefined

getInput :: IO (IntMap [Char], [(Int, (Int, Int))])
getInput = parseInput input $(inputFilePath)

input :: Parser (IntMap [Char], [(Int, (Int, Int))])
input = (,) <$> (stacks <* labels <* newline) <*> some rearrangement
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

rearrange :: IntMap [Char] -> [(Int, (Int, Int))] -> IntMap [Char]
rearrange stacks [] = stacks
rearrange stacks ((howMany, (from, to)) : rearrangements) =
  rearrange (IM.insert from ys (IM.adjust (reverse xs ++) to stacks)) rearrangements
  where
    (xs, ys) = splitAt howMany (stacks ! from)

example :: IO (IntMap [Char], [(Int, (Int, Int))])
example =
  parseString input $
    unlines
      [ "    [D]    ",
        "[N] [C]    ",
        "[Z] [M] [P]",
        " 1   2   3 ",
        "",
        "move 1 from 2 to 1",
        "move 3 from 1 to 3",
        "move 2 from 2 to 1",
        "move 1 from 1 to 2"
      ]
