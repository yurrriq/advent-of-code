{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AdventOfCode.Year2021.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Monoid.Action (Action (..))
import Data.Monoid.SemiDirectProduct.Strict (Semi, embed, inject, untag)
import Data.Semigroup (Sum (..), getSum)
import Linear (V2 (..))
import Text.Trifecta (Parser, natural, some, symbol)

type Direction = Sum (V2 Int)

newtype Aim = Aim Int
  deriving
    (Semigroup, Monoid)
    via (Sum Int)

instance Action Aim Direction where
  act (Aim a) (Sum (V2 x y)) = Sum (V2 x (y + a * x))

main :: IO ()
main = $(defaultMain)

getInput :: IO [Direction]
getInput = parseInput (some direction) $(inputFilePath)

example :: [Direction]
example =
  [ forward 5,
    down 5,
    forward 8,
    up 3,
    down 8,
    forward 2
  ]

partOne :: [Direction] -> Int
partOne = product . getSum . mconcat

partTwo :: [Direction] -> Int
partTwo = product . getSum . untag . mconcat . map transform
  where
    transform :: Direction -> Semi Direction Aim
    transform dir@(Sum (V2 _ 0)) = inject dir
    transform (Sum (V2 0 y)) = embed (Aim y)
    transform _ = error "Invalid direction"

direction :: Parser Direction
direction = dir <*> (fromInteger <$> natural)
  where
    dir =
      symbol "forward" $> forward
        <|> symbol "down" $> down
        <|> symbol "up" $> up

forward, down, up :: Int -> Direction
forward = pure . flip V2 0
down = pure . V2 0
up = down . negate
