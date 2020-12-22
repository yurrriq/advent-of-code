module AdventOfCode.Year2018.Day01
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (findFirstDup, scan)
import Control.Category ((>>>))
import Data.Monoid (Sum (..))
import Text.Trifecta (Parser, integer, some)

newtype FrequencyChange
  = FrequencyChange
      {unFrequencyChange :: Sum Integer}
  deriving (Eq, Ord, Show)

instance Semigroup FrequencyChange where
  (FrequencyChange x) <> (FrequencyChange y) = FrequencyChange (x <> y)

instance Monoid FrequencyChange where
  mempty = FrequencyChange (Sum 0)

main :: IO ()
main = do
  input <- parseInput (some frequencyChange) $(inputFilePath)
  putStr "Part One: "
  print (partOne input)
  putStr "Part Two: "
  putStrLn $ maybe "failed!" show (partTwo input)

frequencyChange :: Parser FrequencyChange
frequencyChange = FrequencyChange . Sum <$> integer

partOne :: [FrequencyChange] -> Integer
partOne = getSum . unFrequencyChange . mconcat

partTwo :: [FrequencyChange] -> Maybe Integer
partTwo =
  scan . cycle
    >>> findFirstDup
    >>> fmap (getSum . unFrequencyChange)
