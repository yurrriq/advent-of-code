{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2025.Day06 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (maybeFail)
import Data.Char (isSpace)
import Data.List (unsnoc)
import Data.List.Split (splitWhen)
import Relude
import Text.Trifecta (Parser, natural, newline, noneOf, sepEndBy1, symbol, whiteSpace)

worksheet :: Parser [String]
worksheet = some (noneOf "\n") `sepEndBy1` newline

number :: Parser Integer
number = whiteSpace *> natural

operation :: Parser ([Integer] -> Integer)
operation = sum <$ symbol "+" <|> symbol "*" $> product

getExample :: IO [String]
getExample = parseString worksheet example

example :: String
example =
  "123 328  51 64 \n\
  \ 45 64  387 23 \n\
  \  6 98  215 314\n\
  \*   +   *   +  "

getInput :: IO [String]
getInput = parseInputAoC 2025 6 worksheet

partOne :: SimplePuzzle [String] Integer
partOne =
  ask >>= maybeFail "empty worksheet" . unsnoc >>= \(nums, ops) -> do
    numberColumns <- transpose <$> traverse (parseString (some number)) nums
    operations <- parseString (some operation) ops
    pure $ sum $ zipWith ($) operations numberColumns

partTwo :: SimplePuzzle [String] Integer
partTwo = asks (splitWhen (all isSpace) . transpose) >>= traverse go <&> sum
  where
    go problem = do
      (lastColumn, columns) <- maybeFail "empty problem" (uncons problem)
      (column, op) <- maybeFail "empty column" (unsnoc lastColumn)
      parseString operation [op]
        <*> traverse (parseString number) (column : columns)

main :: IO ()
main = $(defaultMainPuzzle)
