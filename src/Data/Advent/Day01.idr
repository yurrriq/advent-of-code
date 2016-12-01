||| Day 1: No Time for a Taxicab
module Data.Advent.Day1

import Control.Arrow
import Control.Category
import Data.Morphisms
import Data.SortedSet

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

-- -------------------------------------------------------------- [ Data Types ]

%access public export

data Heading = N | E | S | W

data Direction = L | R

implementation Show Direction where
    show L = "Left"
    show R = "Right"

Instruction : Type
Instruction = (Direction, Integer)

Coordinates : Type
Coordinates = (Integer, Integer)

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
    go : Heading -> Morphism Coordinates Coordinates
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

-- ---------------------------------------------------------------- [ Part One ]

namespace PartOne

    main : IO ()
    main = do Right str <- readFile "input/day1.txt"
                | Left err => printLn err
              case parse instructions str of
                   Right is => printLn (distance is)
                   Left err => printLn err

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
    main = do Right str <- readFile "input/day1.txt"
                | Left err => printLn err
              case parse instructions str of
                   Right is => printLn $ partTwo is (N, (0, 0)) empty
                   Left err => printLn err

-- -------------------------------------------------------------------- [ Main ]

namespace Main

    main : IO ()
    -- main = PartOne.main
    main = PartTwo.main

-- --------------------------------------------------------------------- [ EOF ]
