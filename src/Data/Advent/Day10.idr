-- --------------------------------------------------------------- [ Day10.idr ]
-- Module      : Data.Advent.Day10
-- Description : My solution to the Day 10 puzzle of the 2016 Advent of Code.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : https://adventofcode.com/2016/day/2
-- --------------------------------------------------------------------- [ EOH ]
||| Day 10: Balance Bots
module Data.Advent.Day10

import Data.Advent.Day

import public Data.SortedMap

import Effects
import Effect.State

%default total

-- -------------------------------------------------------------- [ Data Types ]

%access public export

Chip : Type
Chip = Nat

OutBin : Type
OutBin = Nat

BotID : Type
BotID = Nat

data Give = ToBot BotID
          | ToOutBin OutBin

data Instruction = Bot BotID Give Give
                 | GoesTo Chip BotID

-- ----------------------------------------------------------------- [ Parsers ]

%default partial

%access export

nat : Parser Nat
nat = fromIntegerNat <$> integer

toBot : Parser Give
toBot = ToBot <$> (string "bot " *> nat) <?> "bot n"

toOutBin : Parser Give
toOutBin = ToOutBin <$> (string "output " *> nat) <?> "output n"

to : Parser Give
to = toBot <|> toOutBin <?> "{bot,output} n"

value : Parser Instruction
value = [| GoesTo (string "value " *> nat) (string " goes to bot " *> nat) |]

bot : Parser Instruction
bot = [| Bot (string "bot " *> nat)
             (string " gives low to " *> to)
             (string " and high to " *> to) |]

instruction : Parser Instruction
instruction = bot <|> value

-- ------------------------------------------------------------------- [ State ]

%default total

BotResponsibilities : Type
BotResponsibilities = SortedMap BotID (List Chip)

implementation Default BotResponsibilities where default = empty

BotState : Type -> Type
BotState ty = Eff ty [STATE BotResponsibilities]

-- ------------------------------------------------------------------- [ Logic ]

getChips : BotID -> BotState (List Chip)
getChips b = fromMaybe [] . lookup b <$> get

consChip : BotID -> Chip -> BotState ()
consChip b c = update (insert b (c :: !(getChips b)))

giveChip : Give -> Chip -> BotState (List (OutBin, Chip))
giveChip (ToBot b) c    = consChip b c *> pure []
giveChip (ToOutBin o) c = pure [(o, c)]

%default partial

zoom : List Instruction -> BotState (List (OutBin, Chip))
zoom []                    = pure []
zoom (i@(GoesTo c b)::is)  = consChip b c *> zoom is
zoom (i@(Bot b lo hi)::is) =
    do [c1,c2] <- sort <$> getChips b
         | _ => zoom (is ++ [i])
       pure $ !(giveChip lo c1) ++ !(giveChip hi c2) ++ !(zoom is)

-- ----------------------------------------------------------------- [ Example ]

example : BotState (List (OutBin, Chip))
example = zoom [
            GoesTo 5 2,
            Bot 2 (ToBot 1) (ToBot 0),
            GoesTo 3 1,
            Bot 1 (ToOutBin 1) (ToBot 0),
            Bot 0 (ToOutBin 2) (ToOutBin 0),
            GoesTo 2 2
          ]

runExample : List (OutBin, Chip)
runExample = runPure example

-- ---------------------------------------------------------------- [ Part One ]

partOne : List Instruction -> Maybe BotID
partOne is = runPure $ zoom is *> go <$> get
  where
    go : BotResponsibilities -> Maybe BotID
    go st = fst <$> find (\(_,cs) => sort cs == [17,61]) (toList st)

-- ---------------------------------------------------------------- [ Part Two ]

partTwo : List Instruction -> Nat
partTwo is = foldl go 1 (runPure (zoom is))
  where
    go : Nat -> (OutBin, Chip) -> Nat
    go acc (o,c) = if List.elem o [0,1,2] then c * acc else acc

-- -------------------------------------------------------------------- [ Main ]

namespace Main

    main : IO ()
    main = runDay $ MkDay 10 (some (instruction <* newline))
                    (pure . show . partOne)
                    (pure . show . partTwo)

-- --------------------------------------------------------------------- [ EOF ]
