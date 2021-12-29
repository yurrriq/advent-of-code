{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Year2021.Day10 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Data.Either (fromLeft)
import Data.Foldable (foldlM)
import Data.Functor (($>))
import Text.Trifecta (Parser, char, newline, sepEndBy, some)

data Bracket
  = Paren
  | Square
  | Curly
  | Angle
  deriving (Eq, Show)

main :: IO ()
main = $(defaultMain)

getInput :: IO [[Either Bracket Bracket]]
getInput = parseInput (some bracket `sepEndBy` newline) $(inputFilePath)

example :: IO [[Either Bracket Bracket]]
example =
  parseString (some bracket `sepEndBy` newline) . unlines $
    [ "[({(<(())[]>[[{[]{<()<>>",
      "[(()[<>])]({[<{<<[]>>(",
      "{([(<{}[<>[]}>{[]{[(<()>",
      "(((({<>}<{<{<>}{[]{[]{}",
      "[[<[([]))<([[{}[[()]]]",
      "[{[{({}]{}}([{[{{{}}([]",
      "{<[[]]>}<{[{[{[]{()[[[]",
      "[<(<(<(<{}))><([]([]()",
      "<{([([[(<>()){}]>(<<{{",
      "<{([{{}}[<[[[<>{}]]]>[]]"
    ]

partOne :: [[Either Bracket Bracket]] -> Int
partOne = sum . map (fromLeft 0 . foldlM go [])
  where
    go [] next = Right [next]
    go (Left lhs : rest) (Right rhs)
      | lhs == rhs = Right rest
      | otherwise = Left (scoreIllegalBracket rhs)
    go stack next = Right (next : stack)

partTwo :: [[Either Bracket Bracket]] -> Int
partTwo = undefined

bracket :: Parser (Either Bracket Bracket)
bracket =
  char '(' $> Left Paren
    <|> char ')' $> Right Paren
    <|> char '[' $> Left Square
    <|> char ']' $> Right Square
    <|> char '{' $> Left Curly
    <|> char '}' $> Right Curly
    <|> char '<' $> Left Angle
    <|> char '>' $> Right Angle

scoreIllegalBracket :: Bracket -> Int
scoreIllegalBracket = \case
  Paren -> 3
  Square -> 57
  Curly -> 1197
  Angle -> 25137
