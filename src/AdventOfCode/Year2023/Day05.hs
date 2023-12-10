{-# LANGUAGE RecordWildCards #-}

module AdventOfCode.Year2023.Day05 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.Ix (inRange, range)
import Data.List (find)
import Linear.V3 (V3 (..))
import Text.Trifecta hiding (parseString)

main :: IO ()
main = $(defaultMain)

partOne :: Almanac -> Int
partOne (Almanac seedz s2ses s2fs f2ws w2ls l2ts t2hs h2ls) =
  minimum $
    map
      (go h2ls . go t2hs . go l2ts . go w2ls . go f2ws . go s2fs . go s2ses)
      seedz
  where
    go mappings input =
      case find (\(V3 _ src len) -> inRange (src, src + len) input) mappings of
        Just (V3 dst src _) -> input + (dst - src)
        Nothing -> input

partTwo :: Almanac -> Int
partTwo a@Almanac {..} = partOne (a {almanacSeeds = allSeeds})
  where
    allSeeds = concatMap expand (takeNth 2 ((zip <*> tail) almanacSeeds))
    expand (start, len) = range (start, start + len - 1)

getInput :: IO Almanac
getInput = parseInput almanac $(inputFilePath)

data Almanac = Almanac
  { almanacSeeds :: [Int],
    almanacSeedToSoil :: [V3 Int],
    almanacSoilToFertilizer :: [V3 Int],
    almanacFertilizerToWater :: [V3 Int],
    almanacWaterToLight :: [V3 Int],
    almanacLightToTemperature :: [V3 Int],
    almanacTemperatureToHumidity :: [V3 Int],
    almanacHumidityToLocation :: [V3 Int]
  }
  deriving (Show)

almanac :: Parser Almanac
almanac =
  Almanac
    <$> (symbol "seeds:" *> some posInt)
    <*> (symbol "seed-to-soil map:" *> some mapping)
    <*> (symbol "soil-to-fertilizer map:" *> some mapping)
    <*> (symbol "fertilizer-to-water map:" *> some mapping)
    <*> (symbol "water-to-light map:" *> some mapping)
    <*> (symbol "light-to-temperature map:" *> some mapping)
    <*> (symbol "temperature-to-humidity map:" *> some mapping)
    <*> (symbol "humidity-to-location map:" *> some mapping)

mapping :: Parser (V3 Int)
mapping = V3 <$> posInt <*> posInt <*> posInt

posInt :: Parser Int
posInt = fromInteger <$> natural

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

-- | Return a list of every nth element of a given list.
takeNth :: Int -> [a] -> [a]
takeNth _ [] = []
takeNth n xs = head xs : takeNth n (drop n xs)
