{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2021.Day05 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (frequencies)
import Control.Lens (views)
import Data.Map qualified as M
import Linear (R2 (..), V2 (..), (*^), _x, _y)
import Relude
import Text.Printf (printf)
import Text.Show qualified
import Text.Trifecta (Parser, comma, natural, symbol)

data Vent
  = Vent
  { _from :: !(V2 Int),
    _to :: !(V2 Int)
  }
  deriving (Eq, Generic)

instance Show Vent where
  show (Vent (V2 x1 y1) (V2 x2 y2)) =
    printf "%d,%d -> %d,%d" x1 y1 x2 y2

newtype Field
  = Field {unField :: [Vent]}
  deriving (Eq, Generic)

instance Show Field where
  show (Field vents) =
    toString
      $ unlines
        [ foldMap (\x -> maybe "." show (M.lookup (V2 x y) freqs)) [0 .. 9]
        | y <- [0 .. 9]
        ]
    where
      freqs = frequencies (concatMap line vents)

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO Field
getInput = parseInputAoC 2021 5 field

field :: Parser Field
field = Field <$> some vent
  where
    vent = Vent <$> point <* arrow <*> point
    arrow = symbol "->"
    point = V2 <$> int <* comma <*> int
    int = fromInteger <$> natural

getExample :: IO Field
getExample = parseString field example

example :: String
example =
  "0,9 -> 5,9\n\
  \8,0 -> 0,8\n\
  \9,4 -> 3,4\n\
  \2,2 -> 2,1\n\
  \7,0 -> 7,4\n\
  \6,4 -> 2,0\n\
  \0,9 -> 2,9\n\
  \3,4 -> 1,4\n\
  \0,0 -> 8,8\n\
  \5,5 -> 8,2\n"

partOne :: SimplePuzzle Field Int
partOne =
  asks (filter (liftA2 (||) isHorizontal isVertical) . unField) >>= \vents ->
    evalPuzzle (Field vents) mempty partTwo

partTwo :: SimplePuzzle Field Int
partTwo =
  asks (M.size . M.filter (>= 2) . frequencies . concatMap line . unField)

isHorizontal :: Vent -> Bool
isHorizontal (Vent from to) = to - from & views _y (== 0)

isVertical :: Vent -> Bool
isVertical (Vent from to) = to - from & views _x (== 0)

-- Bresenham seems like overkill.
line :: Vent -> [V2 Int]
line (Vent from to) = [from + t *^ step | t <- [0 .. gcf]]
  where
    step = diff <&> (`div` gcf)
    diff@(V2 dx dy) = to - from
    gcf = gcd dx dy
