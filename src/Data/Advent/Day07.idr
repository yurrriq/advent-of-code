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

-- -------------------------------------------------------------- [ Data Types ]

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

-- ----------------------------------------------------------------- [ Parsers ]

%access export

letterExcept : Char -> Parser Char
letterExcept c = satisfy (\x => x /= c && isAlpha x) <?>
                 "a letter that's not " ++ singleton c

abba : Parser ABBA
abba = do a  <- letter
          b  <- letterExcept a
          b' <- char b
          a' <- char a
          pure [a,b,b',a'] <?> "an ABBA"

maybeAbba : Parser (Maybe ABBA)
maybeAbba = Just <$> abba <|>
            (skip letter *> pure Nothing) <?> "maybe an ABBA or skip a letter"

partial
abbas : Parser Sequence
abbas = catMaybes <$> some maybeAbba <?> "a list of ABBAs"

partial
hypernetSequence : Parser Sequence
hypernetSequence =
    between (char '[') (char ']') abbas <?> "a hypernet sequence"

partial
segment : Parser Segment
segment = liftA2 MkPair abbas (hypernetSequence <|> pure []) <?> "a segment"

-- ------------------------------------------------------------------- [ Logic ]

supportsTLS : Address -> Bool
supportsTLS seg = any (not . isNil . fst) seg &&
                  all (isNil . snd) seg

supportTLS : Input -> Nat
supportTLS = List.length . filter id . map supportsTLS

main' : Show a => Parser Input -> (Input -> a) -> IO ()
main' p f =
    either putStrLn (printLn . f)
           !(run $ parseFile (const show) (const id) p "input/day07.txt")

-- ---------------------------------------------------------------- [ Examples ]

||| ```idris example
||| supportTLS <$> parse (some (some segment <* spaces)) examples
||| ```
examples : String
examples = """abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
ioxxoj[asdfgh]zxcvbn"""

-- ---------------------------------------------------------------- [ Part One ]

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
