module AdventOfCode.Year2021.Day08 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.FastDigits (undigits)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.List (permutations)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Text.Trifecta (count, oneOf, some, space, symbol, try)

main :: IO ()
main = $(defaultMain)

partOne :: [(Set (Set Char), [Set Char])] -> Int
partOne = sum . map go
  where
    go = length . filter (`elem` [2, 3, 4, 7]) . map length . snd

partTwo :: [(Set (Set Char), [Set Char])] -> Int
partTwo = sum . map go
  where
    go (patterns, outputs) =
      let displays = problemSpace M.! patterns
       in fromInteger . undigits (10 :: Int) . reverse $
            map (displays M.!) outputs

getInput :: IO [(Set (Set Char), [Set Char])]
getInput = parseInput (some display) $(inputFilePath)
  where
    display =
      do
        connections <- S.fromList <$> count 10 segment
        _ <- symbol "|"
        output <- count 4 segment
        pure (connections, output)
    segment = S.fromList <$> some (oneOf "abcdefg") <* try space

-- | A map from a set of ten signal patterns to a map from signal pattern to digit.
problemSpace :: Map (Set (Set Char)) (Map (Set Char) Int)
problemSpace =
  M.fromList
    [ ( S.fromList $
          (map . S.map) (segmentTranslations M.!) $
            IM.elems referenceDigits,
        M.fromList
          [ (S.map (segmentTranslations M.!) signalPattern, digit)
            | (digit, signalPattern) <- IM.toList referenceDigits
          ]
      )
      | permutation <- permutations "abcdefg",
        let segmentTranslations = M.fromList (zip "abcdefg" permutation)
    ]

-- | A map from digit to canonical signal pattern.
referenceDigits :: IntMap (Set Char)
referenceDigits =
  IM.map S.fromList . IM.fromList $
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
