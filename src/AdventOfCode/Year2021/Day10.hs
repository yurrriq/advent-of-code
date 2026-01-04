{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2021.Day10 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (medianUnsafe)
import Data.Either.Extra (eitherToMaybe)
import Data.Finitary (Finitary (toFinite))
import Data.List ((!!))
import Data.List.Extra (sumOn')
import Relude
import Text.Show qualified
import Text.Trifecta (Parser, char, choice, newline, sepEndBy)

data Bracket
  = Paren
  | Square
  | Curly
  | Angle
  deriving (Eq, Generic)
  deriving anyclass (Finitary)

type Line = [Either Bracket Bracket]

instance {-# OVERLAPPING #-} Show Line where
  show = concatMap (either open close)
    where
      open = pure . ("([{<" !!) . fromIntegral . toFinite
      close = pure . (")]}>" !!) . fromIntegral . toFinite

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO [Line]
getInput = parseInputAoC 2021 10 (some bracket `sepEndBy` newline)

getExample :: IO [Line]
getExample = parseString (some bracket `sepEndBy` newline) example

example :: String
example =
  "[({(<(())[]>[[{[]{<()<>>\n\
  \[(()[<>])]({[<{<<[]>>(\n\
  \{([(<{}[<>[]}>{[]{[(<()>\n\
  \(((({<>}<{<{<>}{[]{[]{}\n\
  \[[<[([]))<([[{}[[()]]]\n\
  \[{[{({}]{}}([{[{{{}}([]\n\
  \{<[[]]>}<{[{[{[]{()[[[]\n\
  \[<(<(<(<{}))><([]([]()\n\
  \<{([([[(<>()){}]>(<<{{\n\
  \<{([{{}}[<[[[<>{}]]]>[]]\n"

partOne :: SimplePuzzle [Line] Int
partOne = asks (sumOn' (fromLeft 0 . scoreLine))

partTwo :: SimplePuzzle [Line] Int
partTwo = asks (medianUnsafe . mapMaybe (eitherToMaybe . scoreLine))

scoreLine :: Line -> Either Int Int
scoreLine = foldlM scoreIncomplete [] >>> second (foldl' scoreCorrupt 0)
  where
    scoreIncomplete [] next = Right [next]
    scoreIncomplete (Left lhs : rest) ket@(Right rhs)
      | lhs == rhs = Right rest
      | otherwise = Left (scoreBracket ket)
    scoreIncomplete stack next = Right (next : stack)

    scoreCorrupt total ket = total * 5 + scoreBracket ket

bracket :: Parser (Either Bracket Bracket)
bracket =
  choice
    [ char '(' $> Left Paren,
      char ')' $> Right Paren,
      char '[' $> Left Square,
      char ']' $> Right Square,
      char '{' $> Left Curly,
      char '}' $> Right Curly,
      char '<' $> Left Angle,
      char '>' $> Right Angle
    ]

scoreBracket :: Either Bracket Bracket -> Int
scoreBracket = either autocomplete illegal
  where
    autocomplete = (1 +) . fromIntegral . toFinite
    illegal = ([3, 57, 1197, 25137] !!) . fromIntegral . toFinite
