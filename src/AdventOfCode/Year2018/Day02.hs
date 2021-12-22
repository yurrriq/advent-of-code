{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2018.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMainMaybe, inputFilePath)
import AdventOfCode.Util (frequencies, hammingSimilar)
import Control.Arrow ((&&&), (***), (>>>))
import Data.List (find, intersect, tails)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Trifecta (letter, newline, sepEndBy, some)

type BoxID = String

type Checksum = Integer

main :: IO ()
main = $(defaultMainMaybe)

getInput :: IO [BoxID]
getInput = parseInput (some letter `sepEndBy` newline) $(inputFilePath)

partOne :: [BoxID] -> Maybe Checksum
partOne = Just . checksum

partTwo :: [BoxID] -> Maybe String
partTwo = fmap (uncurry intersect) . correctBoxIDs

checksum :: [BoxID] -> Checksum
checksum =
  fmap frequencies
    >>> filter (elem 2) &&& filter (elem 3)
    >>> length *** length
    >>> product
    >>> fromIntegral

correctBoxIDs :: [BoxID] -> Maybe (BoxID, BoxID)
correctBoxIDs = listToMaybe . mapMaybe go . tails
  where
    go (x : xs@(_ : _)) = (x,) <$> find (hammingSimilar 1 x) xs
    go _ = Nothing
