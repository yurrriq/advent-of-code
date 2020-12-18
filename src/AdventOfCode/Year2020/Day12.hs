{-# LANGUAGE FlexibleContexts #-}

module AdventOfCode.Year2020.Day12
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Lens
import Data.Functor (($>))
import Linear (V2 (..), _yx, perp)
import Text.Parser.Token.Highlight (Highlight (..))
import Text.Trifecta (Parser, char, highlight, natural, some)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne input
    putStr "Part Two: "
    print $ partTwo input

getInput :: IO [Instruction]
getInput = parseInput (some instruction) $(inputFilePath)

partOne :: [Instruction] -> Int
partOne = solve stepOne (pure 0, E)

partTwo :: [Instruction] -> Int
partTwo = solve stepTwo (pure 0, E, V2 10 (-1))

solve :: Field1 a a (V2 Int) (V2 Int) => (Instruction -> a -> a) -> a -> [Instruction] -> Int
solve f initialState = sum . abs . view _1 . foldl (flip f) initialState

stepOne :: Instruction -> (V2 Int, Heading) -> (V2 Int, Heading)
stepOne (Head b n) = _1 %~ move b n
stepOne (Move F n) = first =<< flip move n . snd
stepOne (Turn d n) = _2 %~ (!! (n `div` 90)) . iterate (turn d)

stepTwo :: Instruction -> (V2 Int, Heading, V2 Int) -> (V2 Int, Heading, V2 Int)
stepTwo (Head b n) = _3 %~ move b n
stepTwo (Move F n) = (_1 %~) =<< (+) . (pure n *) . (^. _3)
stepTwo (Turn d n) = _3 %~ (!! (n `div` 90)) . iterate (rotate d)

move :: Heading -> Int -> V2 Int -> V2 Int
move N n = _2 %~ subtract n
move E n = _1 %~ (n +)
move S n = _2 %~ (n +)
move W n = _1 %~ subtract n

rotate :: Turning -> V2 Int -> V2 Int
rotate L = _yx %~ perp
rotate R = perp

turn :: Turning -> Heading -> Heading
turn L N = W
turn L b = pred b
turn R W = N
turn R b = succ b

data Instruction
  = Head Heading Int
  | Turn Turning Int
  | Move Movement Int
  deriving (Eq, Show)

data Heading = N | E | S | W
  deriving (Eq, Enum, Show)

data Turning = L | R
  deriving (Eq, Show)

data Movement = F
  deriving (Eq, Show)

instruction :: Parser Instruction
instruction = instruct <*> (fromInteger <$> natural)
  where
    instruct =
      Head <$> heading
        <|> Turn <$> turning
        <|> Move <$> movement

heading :: Parser Heading
heading =
  highlight ReservedIdentifier $
    char 'N' $> N
      <|> char 'S' $> S
      <|> char 'E' $> E
      <|> char 'W' $> W

turning :: Parser Turning
turning =
  highlight ReservedIdentifier $
    char 'L' $> L
      <|> char 'R' $> R

movement :: Parser Movement
movement = char 'F' $> F
