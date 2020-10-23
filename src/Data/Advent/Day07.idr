-- --------------------------------------------------------------- [ Day07.idr ]
-- Module      : Data.Advent.Day07
-- Description : My solution to the Day 7 puzzle of the 2016 Advent of Code.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : http://adventofcode.com/2016/day/7
-- --------------------------------------------------------------------- [ EOH ]
||| Day 7: Signals and Noise
module Data.Advent.Day07

import Control.Arrow
import Control.Category
import Data.Morphisms

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

-- ---------------------------------------------------- [ Part Two: Data Types ]

%access public export

ABA : Type
ABA = Vect 3 Char

BAB : Type
BAB = Vect 3 Char

-- ------------------------------------------------------- [ Part Two: Helpers ]

%access export

abaToBab : ABA -> BAB
abaToBab [a,b,_] = [b,a,b]

-- ------------------------------------------------------- [ Part Two: Parsers ]

%access export
%default partial

xyxs : Parser (List (Vect 3 Char))
xyxs = go <$> some letter
  where
    go : List Char -> List ABA
    go (x::t@(y::z::zs)) = let ts = go t in if x == z then [x,y,x] :: ts else ts
    go _                 = []

abas : Parser (List ABA)
abas = xyxs

babs : Parser (List BAB)
babs = xyxs

hypernetSequence' : Parser (List BAB)
hypernetSequence' = between (char '[') (char ']') babs <?> "a hypernet sequence"

segment' : Parser (List ABA, List BAB)
segment' = liftA2 MkPair abas (hypernetSequence' <|> pure []) <?> "a segment"

-- --------------------------------------------------------- [ Part Two: Logic ]

%access export
%default total

supportsSSL : List (List ABA, List BAB) -> Bool
supportsSSL = applyMor $ smash >>> grab
  where
    smash : List (List ABA, List BAB) ~> (List BAB, List BAB)
    smash = arrow (concatMap (map abaToBab . fst)) &&& arrow (concatMap snd)
    grab : (List BAB, List BAB) ~> Bool
    grab = arrow (uncurry (flip (any . flip elem)))

supportSSL : List (List (List ABA, List BAB)) -> Nat
supportSSL = List.length . filter id . map supportsSSL

-- ------------------------------------------------------ [ Part Two: Examples ]

||| ```idris example
||| map supportsSSL <$> parse (some (some segment' <* spaces)) partTwoExamples
||| ```
||| ```idris example
||| supportSSL <$> parse (some (some segment' <* spaces)) partTwoExamples
||| ```
partTwoExamples : String
partTwoExamples = "aba[bab]xyz\nxyx[xyx]xyx\naaa[kek]eke\nzazbz[bzb]cdb"

-- ---------------------------------------------------------- [ Part Two: Main ]

namespace PartTwo

    partial
    main : IO ()
    main = main' (some (some segment' <* spaces)) supportSSL

-- -------------------------------------------------------------------- [ Main ]

namespace Main

    partial
    main : IO ()
    main = putStr "Part One: " *> PartOne.main *>
           putStr "Part Two: " *> PartTwo.main

-- --------------------------------------------------------------------- [ EOF ]
