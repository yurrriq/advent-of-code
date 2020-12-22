{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2018.Day04
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (frequencies)
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Data.List (maximumBy, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Text.Trifecta ((<?>), Parser, brackets, char, colon, many, natural, symbol)

-- ------------------------------------------------------------------- [ Types ]

data Time
  = Time
      { _tYear :: Integer,
        _tMonth :: Integer,
        _tDay :: Integer,
        _tHour :: Integer,
        _tMinute :: Integer
      }
  deriving (Eq, Ord, Show)

time :: Parser Time
time =
  brackets
    ( Time
        <$> ((natural <* char '-') <?> "year")
        <*> ((natural <* char '-') <?> "month")
        <*> (natural <?> "day")
        <*> ((natural <* colon) <?> "hour")
        <*> (natural <?> "minute")
    )
    <?> "time"

newtype Guard = Guard {unGuard :: Integer}
  deriving (Eq, Ord, Show)

data Event
  = Shift Guard
  | Asleep
  | Awake
  deriving (Eq, Ord, Show)

shift :: Parser Event
shift =
  Shift . Guard
    <$> ( symbol "Guard" *> char '#' *> natural
            <* symbol "begins shift"
        )

event :: Parser Event
event =
  shift
    <|> (Asleep <$ symbol "falls asleep")
    <|> (Awake <$ symbol "wakes up")

type Entry = (Time, Event)

entry :: Parser Entry
entry = (,) <$> time <*> event

type TimeCards = Map Guard [[Entry]]

-- ----------------------------------------------------------------- [ Helpers ]

isShiftChange :: Entry -> Bool
isShiftChange (_, Shift _) = True
isShiftChange (_, _) = False

napTimes :: [Entry] -> [Integer]
napTimes ((from, Asleep) : (to, Awake) : rest) =
  [_tMinute from .. _tMinute to - 1] ++ napTimes rest
napTimes _ = []

napsByGuard :: [Entry] -> Map Guard [[Integer]]
napsByGuard = fmap (map napTimes) . shifts

shifts :: [Entry] -> TimeCards
shifts = go Map.empty . sort
  where
    go acc ((_when, Shift who) : entries) =
      let (events, rest) = break isShiftChange entries
       in go (Map.insertWith (++) who [events] acc) rest
    go acc _ = acc

sleepiestGuard :: [Entry] -> Maybe (Int, (Guard, [[Integer]]))
sleepiestGuard = Map.foldrWithKey go Nothing . napsByGuard
  where
    go a xxs Nothing = Just (sum (length <$> xxs), (a, xxs))
    go a xxs old@(Just (m, (_, _))) =
      let n = sum (length <$> xxs)
       in if n > m then Just (n, (a, xxs)) else old

sleepiestMinute :: [[Integer]] -> (Integer, Int)
sleepiestMinute =
  maximumBy (comparing snd)
    . Map.toList
    . frequencies
    . concat

mostConsistentSleeper :: [Entry] -> Maybe ((Guard, Integer), Int)
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

partOne :: [Entry] -> Maybe Integer
partOne entries =
  case sleepiestGuard entries of
    Just (_, (Guard gid, naps)) -> Just (gid * fst (sleepiestMinute naps))
    _ -> Nothing

partTwo :: [Entry] -> Maybe Integer
partTwo entries =
  case mostConsistentSleeper entries of
    Just ((Guard gid, minute), _) -> Just (gid * minute)
    _ -> Nothing

main :: IO ()
main = do
  input <- parseInput (many entry) $(inputFilePath)
  putStr "Part One: "
  putStrLn $ maybe "failed!" show (partOne input)
  putStr "Part Two: "
  putStrLn $ maybe "failed!" show (partTwo input)
