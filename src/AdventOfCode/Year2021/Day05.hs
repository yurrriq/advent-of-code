module AdventOfCode.Year2021.Day05 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (frequencies)
import Control.Applicative (Alternative (..))
import Control.Lens ((^.))
import qualified Data.Map as M
import Linear (V2 (..), (*^), _x, _y)
import Text.Trifecta (comma, natural, symbol)

type Field = [Vent]

type Vent = V2 (V2 Int)

main :: IO ()
main = $(defaultMain)

getInput :: IO [Vent]
getInput = parseInput (some vent) $(inputFilePath)
  where
    vent = V2 <$> point <* arrow <*> point
    arrow = symbol "->"
    point = V2 <$> int <* comma <*> int
    int = fromInteger <$> natural

example :: [Vent]
example =
  [ V2 (V2 0 9) (V2 5 9),
    V2 (V2 8 0) (V2 0 8),
    V2 (V2 9 4) (V2 3 4),
    V2 (V2 2 2) (V2 2 1),
    V2 (V2 7 0) (V2 7 4),
    V2 (V2 6 4) (V2 2 0),
    V2 (V2 0 9) (V2 2 9),
    V2 (V2 3 4) (V2 1 4),
    V2 (V2 0 0) (V2 8 8),
    V2 (V2 5 5) (V2 8 2)
  ]

partOne :: [Vent] -> Int
partOne = partTwo . filter (liftA2 (||) isHorizontal isVertical)

partTwo :: [Vent] -> Int
partTwo = M.size . M.filter (>= 2) . frequencies . concatMap line

isHorizontal :: Vent -> Bool
isHorizontal (V2 from to) = from ^. _y == to ^. _y

isVertical :: Vent -> Bool
isVertical (V2 from to) = from ^. _x == to ^. _x

line :: V2 (V2 Int) -> [V2 Int]
line (V2 from to) = [from + t *^ step | t <- [0 .. gcf]]
  where
    step = (`div` gcf) <$> diff
    diff@(V2 dx dy) = to - from
    gcf = gcd dx dy

{-
bresenham :: V2 (V2 Int) -> [V2 Int]
bresenham (V2 (V2 x1 y1) to@(V2 x2 y2)) = to : unfoldr go (V3 x1 y1 (dx + dy))
  where
    dx = abs (x2 - x1)
    dy = negate (abs (y2 - y1))
    sx = if x1 < x2 then 1 else -1
    sy = if y1 < y2 then 1 else -1
    go st@(V3 x y err)
      | x == x2 && y == y2 = Nothing
      | otherwise = Just (V2 x y, f st)
      where
        e2 = 2 * err
        f =
          bool id ((_y +~ sy) . (_z +~ dx)) (e2 <= dx)
          . bool id ((_x +~ sx) . (_z +~ dy)) (e2 >= dy)
-}

showField :: [Vent] -> String
showField vents =
  unlines
    [ concat
        [ maybe "." show (M.lookup (V2 x y) freqs)
          | x <- [0 .. 9]
        ]
      | y <- [0 .. 9]
    ]
  where
    freqs = frequencies (concatMap line vents)

showVent :: Vent -> String
showVent (V2 (V2 x1 y1) (V2 x2 y2)) =
  show x1
    <> ","
    <> show y1
    <> " -> "
    <> show x2
    <> ","
    <> show y2
