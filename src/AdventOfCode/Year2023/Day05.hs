module AdventOfCode.Year2023.Day05 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.Interval (Extended (..), Interval, (<=..<))
import qualified Data.Interval as Interval
import Data.IntervalMap.Strict (IntervalMap)
import qualified Data.IntervalMap.Strict as IMap
import qualified Data.IntervalSet as ISet
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Text.Trifecta hiding (parseString)

main :: IO ()
main = $(defaultMain)

partOne :: Almanac -> Int
partOne (Almanac (seeds, mappings)) =
  minimum $ map (flip (foldl' convert) mappings) seeds
  where
    convert seed = maybe seed (seed +) . IMap.lookup seed

partTwo :: Almanac -> Int
partTwo (Almanac (seeds, mappings)) =
  fromFinite . Interval.lowerBound . ISet.span $
    flip (foldl' convert) mappings $
      ISet.fromList [mkInterval src len | [src, len] <- chunksOf 2 seeds]
  where
    convert iset imap = unmapped <> mapped
      where
        unmapped = ISet.difference iset (IMap.keysSet imap)
        mapped =
          ISet.fromList
            [ Interval.mapMonotonic (+ delta) interval
              | (interval, delta) <-
                  IMap.toList (IMap.intersectionWith const imap seen)
            ]
        seen = IMap.fromList [(interval, ()) | interval <- ISet.toList iset]

getInput :: IO Almanac
getInput = parseInput almanac $(inputFilePath)

newtype Almanac = Almanac {unAlmanac :: ([Int], [IntervalMap Int Int])}
  deriving (Show)

almanac :: Parser Almanac
almanac =
  do
    seeds <- symbol "seeds:" *> some posInt
    mappings <-
      sequence
        [ symbol "seed-to-soil map:" *> mapping,
          symbol "soil-to-fertilizer map:" *> mapping,
          symbol "fertilizer-to-water map:" *> mapping,
          symbol "water-to-light map:" *> mapping,
          symbol "light-to-temperature map:" *> mapping,
          symbol "temperature-to-humidity map:" *> mapping,
          symbol "humidity-to-location map:" *> mapping
        ]
    pure (Almanac (seeds, mappings))

mapping :: Parser (IntervalMap Int Int)
mapping =
  fmap IMap.fromList . some $
    do
      dst <- posInt
      src <- posInt
      len <- posInt
      pure (mkInterval src len, dst - src)

posInt :: Parser Int
posInt = fromInteger <$> natural

fromFinite :: Extended r -> r
fromFinite (Finite x) = x
fromFinite NegInf = error "left unbounded interval"
fromFinite PosInf = error "empty interval"

mkInterval :: (Ord r, Num r) => r -> r -> Interval r
mkInterval src len = Finite src <=..< Finite (src + len)

getExample :: IO Almanac
getExample = parseString almanac example

example :: String
example =
  "seeds: 79 14 55 13\n\
  \\n\
  \seed-to-soil map:\n\
  \50 98 2\n\
  \52 50 48\n\
  \\n\
  \soil-to-fertilizer map:\n\
  \0 15 37\n\
  \37 52 2\n\
  \39 0 15\n\
  \\n\
  \fertilizer-to-water map:\n\
  \49 53 8\n\
  \0 11 42\n\
  \42 0 7\n\
  \57 7 4\n\
  \\n\
  \water-to-light map:\n\
  \88 18 7\n\
  \18 25 70\n\
  \\n\
  \light-to-temperature map:\n\
  \45 77 23\n\
  \81 45 19\n\
  \68 64 13\n\
  \\n\
  \temperature-to-humidity map:\n\
  \0 69 1\n\
  \1 0 69\n\
  \\n\
  \humidity-to-location map:\n\
  \60 56 37\n\
  \56 93 4\n"
