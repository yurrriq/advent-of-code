{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2018.Day04
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.SimplePuzzle
import AdventOfCode.TH (evalPuzzle)
import AdventOfCode.Util (frequencies)
import Data.List.Extra (maximumOn)
import Data.Map.Strict qualified as Map
import Data.Time (Day, LocalTime (..), TimeOfDay (..), fromGregorian)
import Relude hiding (guard)
import Text.Trifecta (Parser, brackets, char, colon, natural, symbol, (<?>))

-- ------------------------------------------------------- [ Types and Parsers ]

time :: Parser LocalTime
time =
  brackets (LocalTime <$> day <*> timeOfDay)
    <?> "time"

day :: Parser Day
day =
  fromGregorian
    <$> ((natural <* char '-') <?> "year")
    <*> ((posInt <* char '-') <?> "month")
    <*> (posInt <?> "day")

timeOfDay :: Parser TimeOfDay
timeOfDay =
  TimeOfDay
    <$> ((posInt <* colon) <?> "hour")
    <*> (posInt <?> "minute")
    <*> pure 0

posInt :: Parser Int
posInt = fromInteger <$> natural

newtype Guard = Guard {unGuard :: Int}
  deriving (Eq, Ord, Show)

data Event
  = Shift !Guard
  | Asleep
  | Awake
  deriving (Eq, Ord, Show)

event :: Parser Event
event = shift <|> asleep <|> awake
  where
    shift =
      Shift
        . Guard
        <$> (symbol "Guard" *> char '#' *> posInt <* symbol "begins shift")
    asleep = Asleep <$ symbol "falls asleep"
    awake = Awake <$ symbol "wakes up"

type Entry = (LocalTime, Event)

entry :: Parser Entry
entry = (,) <$> time <*> event

type TimeCards = Map Guard [[Entry]]

-- ----------------------------------------------------------------- [ Helpers ]

isShiftChange :: Entry -> Bool
isShiftChange (_, Shift _) = True
isShiftChange (_, _) = False

napTimes :: [Entry] -> [Int]
napTimes ((LocalTime _ from, Asleep) : (LocalTime _ to, Awake) : rest) =
  [todMin from .. todMin to - 1] ++ napTimes rest
napTimes _ = []

napsByGuard :: [Entry] -> Map Guard [[Int]]
napsByGuard = fmap (map napTimes) . shifts

shifts :: [Entry] -> TimeCards
shifts = go Map.empty . sort
  where
    go acc ((_when, Shift who) : entries) =
      let (events, rest) = break isShiftChange entries
       in go (Map.insertWith (++) who [events] acc) rest
    go acc _ = acc

sleepiestGuard :: [Entry] -> Maybe (Int, (Guard, [[Int]]))
sleepiestGuard = Map.foldrWithKey go Nothing . napsByGuard
  where
    go a xxs Nothing = Just (sum (length <$> xxs), (a, xxs))
    go a xxs old@(Just (m, (_, _))) =
      let n = sum (length <$> xxs)
       in if n > m then Just (n, (a, xxs)) else old

sleepiestMinute :: [[Int]] -> (Int, Int)
sleepiestMinute =
  maximumOn snd
    . Map.toList
    . frequencies
    . concat

mostConsistentSleeper :: [Entry] -> Maybe ((Guard, Int), Int)
mostConsistentSleeper =
  Map.foldrWithKey go Nothing
    . fmap (filter (not . null))
    . napsByGuard
  where
    go _ [] old = old
    go guard naps Nothing = Just $ first (guard,) (sleepiestMinute naps)
    go guard naps old@(Just ((_, _), oldTimes)) =
      let (minute, times) = sleepiestMinute naps
       in if times > oldTimes
            then Just ((guard, minute), times)
            else old

-- ------------------------------------------------------------------- [ Parts ]

partOne :: SimplePuzzle [Entry] Int
partOne = do
  Just (_, (Guard gid, naps)) <- asks sleepiestGuard
  pure (gid * fst (sleepiestMinute naps))

partTwo :: SimplePuzzle [Entry] Int
partTwo = do
  Just ((Guard gid, minute), _) <- asks mostConsistentSleeper
  pure (gid * minute)

getInput :: IO [Entry]
getInput = parseInputAoC 2018 4 (many entry)

main :: IO ()
main = $(evalPuzzle)
