{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2020.Day18
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseString, rawInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Relude
import Text.Trifecta (Parser, natural, parens, symbol)

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle String Integer
partOne = solve [[multiplication, addition]]

partTwo :: SimplePuzzle String Integer
partTwo = solve [[addition], [multiplication]]

getInput :: IO String
getInput = rawInputAoC 2020 18

solve :: [[Operator Parser Integer]] -> SimplePuzzle String Integer
solve table = ask >>= parseString (some expr) <&> sum
  where
    expr = makeExprParser (parens expr <|> natural) table

addition :: (Num a) => Operator Parser a
addition = binary "+" (+)

multiplication :: (Num a) => Operator Parser a
multiplication = binary "*" (*)

binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)
