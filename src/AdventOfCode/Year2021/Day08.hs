module AdventOfCode.Year2021.Day08 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util ((<||>))
import Data.FastDigits (undigits)
import Data.Function (on)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (find, sort, (\\))
import Data.List.Ordered (isect)
import qualified Data.Map.Strict as M
import Data.Tuple (swap)
import Text.Trifecta (count, oneOf, some, space, symbol, try)

main :: IO ()
main = $(defaultMain)

partOne :: [([String], [String])] -> Int
partOne = sum . map go
  where
    go = length . filter (`elem` [2, 3, 4, 7]) . map length . snd

partTwo :: [([String], [String])] -> Int
partTwo = sum . map go
  where
    go (patterns, outputs) =
      fromInteger . undigits (10 :: Int) . reverse $
        map (digits M.!) outputs
      where
        digits =
          M.fromList . map swap . IM.toList $
            foldl
              (flip ($ patterns))
              (findEasyNumbers patterns)
              [ deduceNumber 3 1 2,
                deduceNumber 2 4 2,
                deduceNumber 5 2 3,
                deduceNumber 9 3 5,
                deduceNumber 0 1 2,
                deduceNumber 6 0 5
              ]

getInput :: IO [([String], [String])]
getInput = parseInput (some display) $(inputFilePath)
  where
    display =
      do
        connections <- count 10 segment
        _ <- symbol "|"
        output <- count 4 segment
        pure (connections, output)
    segment = sort <$> some (oneOf "abcdefg") <* try space

-- | Given a list of signal patterns, find the representations of digits with
-- a unique number of segments. Return a map from digit to segments.
findEasyNumbers :: [String] -> IntMap String
findEasyNumbers = foldl go IM.empty
  where
    go found pat =
      maybe found (\n -> IM.insert n pat found) $
        IM.lookup (length pat) easy

-- | A map from number of segments used to the digit they represent.
-- All digits are uniqe in the number of segments required.
easy :: IntMap Int
easy = IM.foldlWithKey go IM.empty referenceDigits
  where
    go acc n pat =
      if all ((pat ==) <||> ((/=) `on` length) pat) referenceDigits
        then IM.insert (length pat) n acc
        else acc

deduceNumber :: Int -> Int -> Int -> [String] -> IntMap String -> IntMap String
deduceNumber target source overlaps patterns found =
  let len = length (referenceDigits IM.! target)
   in maybe found (\pat -> IM.insert target pat found) $
        find ((== overlaps) . countOverlaps found source) $
          filter ((== len) . length) $
            patterns \\ IM.elems found

countOverlaps :: IntMap String -> Int -> String -> Int
countOverlaps found known str = length $ isect str $ found IM.! known

-- | A map from digit to canonical (sorted) segment representation.
referenceDigits :: IntMap String
referenceDigits =
  IM.fromList
    [ (0, "abcefg"),
      (1, "cf"),
      (2, "acdeg"),
      (3, "acdfg"),
      (4, "bcdf"),
      (5, "abdfg"),
      (6, "abdefg"),
      (7, "acf"),
      (8, "abcdefg"),
      (9, "abcdfg")
    ]
