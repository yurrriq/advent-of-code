-- --------------------------------------------------------------- [ Day01.idr ]
-- Module      : Data.Advent.Day01
-- Description : My solution to the Day 1 puzzle of the 2016 Advent of Code.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : http://adventofcode.com/2016/day/1
-- --------------------------------------------------------------------- [ EOH ]
||| Day 1: No Time for a Taxicab
module Data.Advent.Day01

import Control.Arrow
import Control.Category
import Data.Morphisms
import Data.SortedSet

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

-- -------------------------------------------------------------- [ Data Types ]

%access public export

||| A cardinal heading.
data Heading = ||| North
               N
             | ||| East
               E
             | ||| South
               S
             | ||| West
               W

||| A direction to walk in, left or right.
data Direction = ||| Left
                 L
               | ||| Right
                 R

implementation Show Direction where
    show L = "Left"
    show R = "Right"

||| A direction and a number of blocks.
Instruction : Type
Instruction = (Direction, Integer)

||| A pair of coordinates on the street grid, `(x, y)`.
Coordinates : Type
Coordinates = (Integer, Integer)

||| A heading and coordinates.
Position : Type
Position = (Heading, Coordinates)

-- -----------------------------------------------------------------------------

%access export

-- ----------------------------------------------------------------- [ Parsers ]

direction : Parser Direction
direction = (char 'L' *> pure L) <|> (char 'R' *> pure R) <?> "direction"

instruction : Parser Instruction
instruction = liftA2 MkPair direction integer <?> "instruction"

instructions : Parser (List Instruction)
instructions =
    commaSep instruction <* spaces <* eof <?> "comma-separated instructions"

-- ------------------------------------------------------------------- [ Logic ]

turnRight : Heading -> Heading
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

turnLeft : Heading -> Heading
turnLeft N = W
turnLeft E = N
turnLeft S = E
turnLeft W = S

turn : Direction -> Heading -> Heading
turn L = turnLeft
turn R = turnRight

-- TODO: Clean this up.
move : Integer -> Position -> Position
move n state@(h, _) = applyMor (second (go h)) state
  where
    go : Heading -> Coordinates ~> Coordinates
    go N = second (arrow (+ n))
    go E = first  (arrow (+ n))
    go S = second (arrow (flip (-) n))
    go W = first  (arrow (flip (-) n))

follow : Instruction -> Position -> Position
follow (dir, len) = applyMor $ first (arrow (turn dir)) >>> arrow (move len)

distance' : Coordinates -> Integer
distance' = abs . uncurry (+)

distance : List Instruction -> Integer
distance = distance' . snd . foldl (flip follow) (N, (0, 0))

main' : (List Instruction -> IO ()) -> IO ()
main' f = do Right str <- readFile "input/day01.txt"
               | Left err => printLn err
             case parse instructions str of
                  Right is => f is
                  Left err => printLn err

-- ---------------------------------------------------------------- [ Part One ]

namespace PartOne

    main : IO ()
    main = main' (printLn . distance)

-- ---------------------------------------------------------------- [ Part Two ]

namespace PartTwo

    partTwo : List Instruction -> Position -> SortedSet Coordinates ->
              Maybe Integer
    partTwo [] _ _                           = Nothing
    partTwo ((dir, len) :: is) (h, loc) seen =
        either (Just . distance')
               (partTwo is (follow (dir, len) (h, loc)))
               (foldl go (Right seen) [1..len])
      where
        go : Either Coordinates (SortedSet Coordinates) -> Integer ->
             Either Coordinates (SortedSet Coordinates)
        go dup@(Left _) _  = dup
        go (Right seen') n = let (_, loc') = follow (dir, n) (h, loc) in
                                 if contains loc' seen'
                                    then Left loc'
                                    else Right $ insert loc' seen'

    main : IO ()
    main = main' $ \is => case partTwo is (N, (0, 0)) empty of
                               Nothing     => putStrLn "Failed!"
                               Just answer => printLn answer

-- -------------------------------------------------------------------- [ Main ]

namespace Main

    main : IO ()
    main = putStr "Part One: " *> PartOne.main *>
           putStr "Part Two: " *> PartTwo.main

-- --------------------------------------------------------------------- [ EOF ]
