-- --------------------------------------------------------------- [ Day02.idr ]
-- Module      : Data.Advent.Day02
-- Description : My solution to the Day 2 puzzle of the 2016 Advent of Code.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : http://adventofcode.com/2016/day/2
-- --------------------------------------------------------------------- [ EOH ]
||| Day 2: Bathroom Security
module Data.Advent.Day02

import public Data.Ix

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

partial
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

partial
main' : (List (List Instruction) -> IO ()) -> IO ()
main' f = do Right str <- readFile "input/day02.txt"
               | Left err => printLn err
             case parse (some instructions) str of
                  Right is => f is
                  Left err => printLn err

-- ---------------------------------------------------------------- [ Part One ]

namespace PartOne

    ||| ```idris example
    ||| example
    ||| ```
    partial
    example : String
    example = fromEither $ buttons <$>
              parse (some instructions) "ULL\nRRDDD\nLURDL\nUUUUD"

    partial
    main : IO ()
    main = main' (putStrLn . buttons)

namespace PartTwo

    keypad : Vect 5 (n ** Vect n Char)
    keypad = [ (1 **           ['1'])
             , (3 **      ['2', '3', '4'])
             , (5 ** ['5', '6', '7', '8', '9'])
             , (3 **      ['A', 'B', 'C'])
             , (1 **           ['D'])
             ]

    -- NOTE: This will wrap at the bounds, which might be unexpected.
    partial
    convert : (n : Nat) -> Fin m -> Fin n
    convert (S j) fm {m} =
        let delta = half $ if S j > m then S j `minus` m else m `minus` S j in
            the (Fin (S j)) $ fromNat $ finToNat fm `f` delta
      where
        f : Nat -> Nat -> Nat
        f = if S j > m then plus else minus
        partial
        half : Nat -> Nat
        half = flip div 2

    canMoveVertically : (Fin (S k), Fin 5) ->
                        (i : Instruction) ->
                        Bool
    canMoveVertically (x, y) i with ((finToNat x, finToNat y))
      canMoveVertically (x, y) U | (col, row) =
          case row of
               Z                   => False
               S Z                 => col == 1
               S (S Z)             => inRange (1,3) col
               _                   => True
      canMoveVertically (x, y) D | (col, row) =
          case row of
               S (S Z)             => inRange (1,3) col
               S (S (S Z))         => col == 1
               S (S (S (S Z)))     => False
               _                   => True
      canMoveVertically _ _ | _ = True

    partial
    move : (Fin (S k), Fin 5) ->
           (i : Instruction) ->
           ((n ** Fin n), Fin 5)
    move (x, y) U = if canMoveVertically (x, y) U
                       then let n = fst (index (pred y) keypad) in
                                ((n ** convert n x), pred y)
                       else ((_ ** x), y)
    move (x, y) D = if canMoveVertically (x, y) D
                       then let n = fst (index (succ y) keypad) in
                                ((n ** convert n x), succ y)
                       else ((_ ** x), y)
    move (x, y) L = let n = fst (index y keypad) in
                        ((n ** convert n (pred x)), y)
    move (x, y) R = let n = fst (index y keypad) in
                        ((n ** convert n (succ x)), y)

    partial
    button : (Fin (S k), Fin 5) ->
             List Instruction ->
             (((n ** Fin n), Fin 5), Char)
    button loc@(x, y) [] =
        let (n ** row) = index y PartTwo.keypad
            xx = convert n x in
            (((n ** xx), y), index xx row)
    button loc (i :: is) =
        let ((S _ ** x), y) = move loc i in
            button (x, y) is

    partial
    buttons : List (List Instruction) -> String
    buttons = go (((5 ** 0),2), [])
      where
        partial
        go : (((n ** Fin n), Fin 5), List Char) ->
             List (List Instruction) ->
             String
        go (_, cs) []            = pack $ reverse cs
        go (loc, cs) (is :: iis) =
            let ((S k ** xx), y) = loc
                (loc', c) = PartTwo.button (xx, y) {k=k} is in
                go (loc', c :: cs) iis

    ||| ```idris example
    ||| PartTwo.example
    ||| ```
    partial
    example : String
    example = fromEither $ PartTwo.buttons <$>
              parse (some instructions) "ULL\nRRDDD\nLURDL\nUUUUD"

    partial
    main : IO ()
    main = main' (putStrLn . PartTwo.buttons)

-- -------------------------------------------------------------------- [ Main ]

namespace Main

    partial
    main : IO ()
    main = putStr "Part One: " *> PartOne.main *>
           putStr "Part Two: " *> PartTwo.main

-- --------------------------------------------------------------------- [ EOF ]
