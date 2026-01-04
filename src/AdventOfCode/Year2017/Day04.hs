{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2017.Day04 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.TH (defaultMain)
import AdventOfCode.Util (count)
import Data.List (nub)
import Relude
import Text.Trifecta (char, letter, newline, sepBy)

type Passphrase = [String]

main :: IO ()
main = $(defaultMain)

getInput :: IO [Passphrase]
getInput = parseInputAoC 2017 4 (some passphrase)
  where
    passphrase = some letter `sepBy` char ' ' <* newline

partOne :: [Passphrase] -> Int
partOne = asks (count isValid)
  where
    isValid passphrase =
      ((==) `on` length) passphrase (nub passphrase)

partTwo :: [Passphrase] -> Int
partTwo = count isValid
  where
    isValid passphrase = sorted == nub sorted
      where
        sorted = sort $ map sort passphrase
