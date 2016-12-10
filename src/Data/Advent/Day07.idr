-- --------------------------------------------------------------- [ Day07.idr ]
-- Module      : Data.Advent.Day07
-- Description : My solution to the Day 7 puzzle of the 2016 Advent of Code.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : http://adventofcode.com/2016/day/7
-- --------------------------------------------------------------------- [ EOH ]
||| Day 7: Signals and Noise
module Data.Advent.Day07

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings
import public Lightyear.StringFile

%default total

-- -------------------------------------------------------------- [ Main Logic ]

%access export

main' : Show b => Parser a -> (a -> b) -> IO ()
main' p f =
    either putStrLn (printLn . f)
           !(run $ parseFile (const show) (const id) p "input/day07.txt")

-- ---------------------------------------------------- [ Part One: Data Types ]

%access public export

ABBA : Type
ABBA = Vect 4 Char

Sequence : Type
Sequence = List ABBA

Segment : Type
Segment = (Sequence, Sequence)

Address : Type
Address = List Segment

Input : Type
Input = List Address

-- ------------------------------------------------------- [ Part One: Helpers ]

%access private

letterExcept : Char -> Parser Char
letterExcept c = satisfy (\x => x /= c && isAlpha x) <?>
                 "a letter that's not " ++ singleton c

maybeSkipLetter : Parser a -> Parser (Maybe a)
maybeSkipLetter p = Just <$> p <|> (skip letter *> pure Nothing)

-- ------------------------------------------------------- [ Part One: Parsers ]

%access export
%default partial

total
abba : Parser ABBA
abba = do a  <- letter
          b  <- letterExcept a
          char b
          char a
          pure [a,b,b,a] <?> "an ABBA"

total
maybeAbba : Parser (Maybe ABBA)
maybeAbba = maybeSkipLetter abba <?> "maybe an ABBA or skip a letter"

abbas : Parser Sequence
abbas = catMaybes <$> some maybeAbba <?> "a list of ABBAs"

hypernetSequence : Parser Sequence
hypernetSequence = between (char '[') (char ']') abbas <?> "a hypernet sequence"

segment : Parser Segment
segment = liftA2 MkPair abbas (hypernetSequence <|> pure []) <?> "a segment"

-- --------------------------------------------------------- [ Part One: Logic ]

%default total

supportsTLS : Address -> Bool
supportsTLS seg = any (not . isNil . fst) seg &&
                  all (isNil . snd) seg

supportTLS : Input -> Nat
supportTLS = List.length . filter id . map supportsTLS

-- ------------------------------------------------------ [ Part One: Examples ]

||| ```idris example
||| map supportsTLS <$> parse (some (some segment <* spaces)) partOneExamples
||| ```
||| ```idris example
||| supportTLS <$> parse (some (some segment <* spaces)) partOneExamples
||| ```
partOneExamples : String
partOneExamples =
    "abba[mnop]qrst\nabcd[bddb]xyyx\naaaa[qwer]tyui\nioxxoj[asdfgh]zxcvbn"

-- ---------------------------------------------------------- [ Part One: Main ]

namespace PartOne

    partial
    main : IO ()
    main = main' (some (some segment <* spaces)) supportTLS


-- -------------------------------------------------------------------- [ Main ]

namespace Main

    partial
    main : IO ()
    main = putStr "Part One: " *> PartOne.main

-- --------------------------------------------------------------------- [ EOF ]
