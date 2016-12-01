||| Day 1: No Time for a Taxicab
module Data.Advent.Day1

import Control.Arrow
import Control.Category
import Data.Morphisms

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

%access export

-- -------------------------------------------------------------- [ Data Types ]

public export
data Heading = N | E | S | W

public export
data Direction = L | R

implementation Show Direction where
    show L = "Left"
    show R = "Right"

-- ----------------------------------------------------------------- [ Parsers ]

direction : Parser Direction
direction = (char 'L' *> pure L) <|> (char 'R' *> pure R) <?> "direction"

instruction : Parser (Direction, Integer)
instruction = liftA2 MkPair direction integer <?> "instruction"

instructions : Parser (List (Direction, Integer))
instructions = commaSep instruction <?> "comma-separated list of instructions"

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
move : Integer -> (Heading, (Integer, Integer)) -> (Heading, (Integer, Integer))
move n state = applyMor (second (go (fst state))) state
  where
    go N = second (arrow (+ n))
    go E = first  (arrow (+ n))
    go S = second (arrow (flip (-) n))
    go W = first  (arrow (flip (-) n))

follow : (Direction, Integer) ->
         (Heading, (Integer, Integer)) ->
         (Heading, (Integer, Integer))
follow (dir, len) = applyMor $ first (arrow (turn dir)) >>> arrow (move len)

distance : List (Direction, Integer) -> Integer
distance = abs . uncurry (+) . snd . foldl (flip follow) (N, (0, 0))

-- ---------------------------------------------------------------- [ Part One ]

namespace PartOne

    main : IO ()
    main = do Right str <- readFile "input/day1.txt"
                | Left err => printLn err
              case parse instructions str of
                   Right is => printLn (distance is)
                   Left err => printLn err

-- --------------------------------------------------------------------- [ EOF ]
