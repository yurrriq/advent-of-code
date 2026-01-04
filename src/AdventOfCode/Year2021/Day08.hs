{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2021.Day08 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Data.FastDigits (undigits)
import Data.IntMap qualified as IM
import Data.List.Extra (sumOn')
import Data.Map qualified as M
import Data.Set qualified as S
import Relude
import Text.Trifecta (count, oneOf, symbol, token)

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle [(Set (Set Char), [Set Char])] Int
partOne = asks (sumOn' go)
  where
    go = length . filter (`elem` [2, 3, 4, 7]) . map length . snd

partTwo :: SimplePuzzle [(Set (Set Char), [Set Char])] Integer
partTwo = asks (sumOn' go)
  where
    go (patterns, outputs) =
      undigits @Int 10
        . reverse
        $ map ((problemSpace M.! patterns) M.!) outputs

getInput :: IO [(Set (Set Char), [Set Char])]
getInput = parseInputAoC 2021 8 (some display)
  where
    display =
      (,)
        <$> (S.fromList <$> count 10 segment <* symbol "|")
        <*> count 4 segment
    segment = S.fromList <$> token (some (oneOf "abcdefg"))

-- | A map from a set of ten signal patterns to a map from signal pattern to digit.
problemSpace :: Map (Set (Set Char)) (Map (Set Char) Int)
problemSpace =
  M.fromList
    [ ( S.fromList
          $ (map . S.map) (segmentTranslations M.!)
          $ IM.elems referenceDigits,
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
  IM.map S.fromList
    . IM.fromList
    $ [ (0, "abcefg"),
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
