-- --------------------------------------------------------------- [ Day02.idr ]
-- Module      : Data.Advent.Day02
-- Description : My solution to the Day 2 puzzle of the 2016 Advent of Code.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : http://adventofcode.com/2016/about
-- --------------------------------------------------------------------- [ EOH ]
||| Day 2: Bathroom Security
module Data.Advent.Day02

import Data.Vect

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

-- -------------------------------------------------------------- [ Data Types ]

%access public export

||| Up, down, left or right.
data Instruction = ||| Up
                   U
                 | ||| Down
                   D
                 | ||| Left
                   L
                 | ||| Right
                   R

||| A single digit, i.e. a number strictly less than ten.
Digit : Type
Digit = Fin 10

implementation Show Digit where
    show = show . finToInteger

implementation [showDigits] Show (List Digit) where
    show = concatMap show

||| A pair of coordinates on the keypad, `(x, y)`.
Coordinates : Type
Coordinates = (Fin 3, Fin 3)

-- -----------------------------------------------------------------------------

%access export

-- ------------------------------------------------------------------ [ Keypad ]

||| A keypad like this:
|||
||| ```
||| 1 2 3
||| 4 5 6
||| 7 8 9
||| ```
keypad : Vect 3 (Vect 3 Digit)
keypad = [ [1, 2, 3],
           [4, 5, 6],
           [7, 8, 9] ]

-- ----------------------------------------------------------------- [ Parsers ]

up : Parser Instruction
up = char 'U' *> pure U <?> "up"

down : Parser Instruction
down = char 'D' *> pure D <?> "down"

left : Parser Instruction
left = char 'L' *> pure L <?> "left"

right : Parser Instruction
right = char 'R' *> pure R <?> "right"

instruction : Parser Instruction
instruction = up <|> down <|> left <|> right <?> "up, down, left or right"

instructions : Parser (List Instruction)
instructions = some instruction <* (skip endOfLine <|> eof)

-- ------------------------------------------------------------------- [ Logic ]

move : Coordinates -> Instruction -> Coordinates
move (x, y) U = (x, pred y)
move (x, y) D = (x, succ y)
move (x, y) L = (pred x, y)
move (x, y) R = (succ x, y)

button : Coordinates -> List Instruction -> (Coordinates, Digit)
button loc@(x, y) [] = (loc, index x (index y keypad))
button loc (i :: is) = button (move loc i) is

buttons : List (List Instruction) -> String
buttons = show @{showDigits} . go ((1,1), [])
  where
    go : (Coordinates, List Digit) -> List (List Instruction) -> List Digit
    go (_, ds) []            = reverse ds
    go (loc, ds) (is :: iis) = let (loc', d) = button loc is in
                                    go (loc', d :: ds) iis

main' : (List (List Instruction) -> IO ()) -> IO ()
main' f = do Right str <- readFile "input/day02.txt"
               | Left err => printLn err
             case parse (some instructions) str of
                  Right is => f is
                  Left err => printLn err

-- ----------------------------------------------------------------- [ Example ]

||| ```idris example
||| example
||| ```
example : String
example = fromEither $ buttons <$>
          parse (some instructions) "ULL\nRRDDD\nLURDL\nUUUUD"

-- ---------------------------------------------------------------- [ Part One ]

namespace PartOne

    main : IO ()
    main = main' (putStrLn . buttons)

-- -------------------------------------------------------------------- [ Main ]

namespace Main

    main : IO ()
    main = putStr "Part One: " *> PartOne.main

-- --------------------------------------------------------------------- [ EOF ]
