module AdventOfCode.Year2017.Day04 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (count)
import Data.Function (on)
import Data.List (nub, sort)
import Text.Trifecta (char, letter, newline, sepBy, some)

type Passphrase = [String]

main :: IO ()
main = $(defaultMain)

getInput :: IO [Passphrase]
getInput = parseInput (some (passphrase <* newline)) $(inputFilePath)
  where
    passphrase = some letter `sepBy` char ' '

partOne :: [Passphrase] -> Int
partOne = count isValid
  where
    isValid passphrase =
      ((==) `on` length) passphrase (nub passphrase)

partTwo :: [Passphrase] -> Int
partTwo = count isValid
  where
    isValid passphrase = sorted == nub sorted
      where
        sorted = sort $ map sort passphrase
