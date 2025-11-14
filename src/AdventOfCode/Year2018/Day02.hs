{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2018.Day02 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.SimplePuzzle
import AdventOfCode.TH (evalPuzzle)
import AdventOfCode.Util (count, frequencies, hammingSimilar, maybeFail)
import Data.List (intersect)
import Relude
import Text.Trifecta (letter, newline, sepEndBy)

type BoxID = String

type Checksum = Integer

main :: IO ()
main = $(evalPuzzle)

getInput :: IO [BoxID]
getInput = parseInputAoC 2018 2 (some letter `sepEndBy` newline)

partOne :: SimplePuzzle [BoxID] Checksum
partOne = asks checksum

partOneExample :: IO Checksum
partOneExample = flip evalPart partOne =<< getExampleOne

partTwo :: SimplePuzzle [BoxID] String
partTwo = ask >>= maybeFail "ope!" . fmap (uncurry intersect) . correctBoxIDs

partTwoExample :: IO String
partTwoExample = flip evalPart partTwo =<< getExampleTwo

checksum :: [BoxID] -> Checksum
checksum =
  map frequencies
    >>> count (elem 2)
    &&& count (elem 3)
    >>> uncurry (*)
    >>> fromIntegral

correctBoxIDs :: [BoxID] -> Maybe (BoxID, BoxID)
correctBoxIDs = listToMaybe . mapMaybe go . tails
  where
    go (x : xs@(_ : _)) = (x,) <$> find (hammingSimilar 1 x) xs
    go _ = Nothing

getExample :: String -> IO [BoxID]
getExample = parseString (some letter `sepEndBy` newline)

getExampleOne :: IO [BoxID]
getExampleOne = getExample exampleOne

exampleOne :: String
exampleOne =
  "abcdef\n\
  \bababc\n\
  \abbcde\n\
  \abcccd\n\
  \aabcdd\n\
  \abcdee\n\
  \ababab\n"

getExampleTwo :: IO [BoxID]
getExampleTwo = getExample exampleTwo

exampleTwo :: String
exampleTwo =
  "abcde\n\
  \fghij\n\
  \klmno\n\
  \pqrst\n\
  \fguij\n\
  \axcye\n\
  \wvxyz\n"
