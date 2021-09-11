module AdventOfCode.Year2017.Day03
  ( main,
    getInput,
    partOne,
  )
where

import Control.Arrow ((&&&), (>>>))
import Control.Lens (view)
import Data.Function (on)
import Linear

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: " *> print (partOne input)

getInput :: IO Int
getInput = pure 265149

partOne :: Int -> Int
partOne = manhattanDistance (pure 0) . (coords !!) . pred

coords :: [V2 Int]
coords = iterate next (pure 0)

next :: V2 Int -> V2 Int
next (V2 0 0) = V2 1 0
next (V2 x y) =
  case (compare x y, signum x, signum y) of
    (GT, _, -1) ->
      if 1 <= x - abs y
        then V2 x (succ y)
        else V2 (succ x) y
    (GT, _, _) -> V2 x (succ y)
    (EQ, -1, _) -> V2 (succ x) y
    (EQ, _, _) -> V2 (pred x) y
    (LT, -1, _) ->
      if 1 <= y - abs x
        then V2 (pred x) y
        else V2 x (pred y)
    (LT, _, _) -> V2 (pred x) y

manhattanDistance :: V2 Int -> V2 Int -> Int
manhattanDistance = curry $ (distanceOn _x &&& distanceOn _y) >>> uncurry (+)
  where
    distanceOn l = abs . uncurry (subtract `on` view l)
