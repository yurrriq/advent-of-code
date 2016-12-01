||| Day 1: No Time for a Taxicab
module Day1

import Control.Arrow
import Control.Category
import Data.Morphisms

data Heading = N | E | S | W

data Direction = L | R

partial
fromString : String -> Direction
fromString "L" = L
fromString "R" = R

parse : String -> (Direction, Int)
parse = applyMor (first strToDir >>> second strToInt) . span isAlpha
  where
    strToDir : Morphism String Direction
    strToDir = arrow fromString   
    strToInt : Morphism String Int
    strToInt = arrow (span isDigit) >>> arrow (cast . fst)

move : (Heading, Int, Int) -> (Direction, Int) -> (Heading, Int, Int)
move (N, (x, y)) (L, n) = (W, (x - n, y))
move (N, (x, y)) (R, n) = (E, (x + n, y))
move (E, (x, y)) (L, n) = (N, (x, y + n))
move (E, (x, y)) (R, n) = (S, (x, y - n))
move (S, (x, y)) (L, n) = (E , (x + n, y))
move (S, (x, y)) (R, n) = (W , (x - n, y))
move (W, (x, y)) (L, n) = (S , (x, y - n))
move (W, (x, y)) (R, n) = (N , (x, y + n))

distance : List String -> Int
distance = abs . uncurry (+) . snd . foldl ((. parse) . move) (N, (0, 0))

main : IO ()
main =
    do Right directions <- map words <$> readFile "input/day1.txt"
         | Left err => printLn err
       printLn $ distance directions
