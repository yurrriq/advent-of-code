module AdventOfCode.Year2015.Day02
  ( main,
    partOne,
    partTwo,
    getInput,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Data.List.Extra (sumOn')
import Linear (V3 (..))
import Text.Trifecta (char, natural, some)

main :: IO ()
main = $(defaultMain)

partOne :: [V3 Integer] -> Integer
partOne = sumOn' paperNeededFor

partTwo :: [V3 Integer] -> Integer
partTwo = sumOn' ribbonNeededFor

getInput :: IO [V3 Integer]
getInput = parseInput (some box) $(inputFilePath)
  where
    box =
      V3
        <$> (natural <* char 'x')
        <*> (natural <* char 'x')
        <*> natural

paperNeededFor :: (Ord a, Num a) => V3 a -> a
paperNeededFor (V3 l w h) = minimum sides + 2 * sum sides
  where
    sides = V3 (l * w) (w * h) (h * l)

ribbonNeededFor :: (Ord a, Num a) => V3 a -> a
ribbonNeededFor dimensions =
  product dimensions + 2 * (sum dimensions - maximum dimensions)
