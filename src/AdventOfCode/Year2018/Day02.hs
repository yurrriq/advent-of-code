module AdventOfCode.Year2018.Day02
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (commonElems, frequencies, hammingSimilar)
import Control.Arrow ((&&&), (***), (>>>))
import Data.List (find, tails)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Trifecta (Parser, letter, newline, sepEndBy, some)

type BoxID = String

boxID :: Parser BoxID
boxID = some letter

type Checksum = Integer

checksum :: [BoxID] -> Checksum
checksum =
  fmap frequencies
    >>> filter (elem 2) &&& filter (elem 3)
    >>> length *** length
    >>> product
    >>> fromIntegral

partOne :: [BoxID] -> Checksum
partOne = checksum

correctBoxIDs :: [BoxID] -> Maybe (BoxID, BoxID)
correctBoxIDs = listToMaybe . mapMaybe go . tails
  where
    go :: [BoxID] -> Maybe (BoxID, BoxID)
    go (x : xs@(_ : _)) = (,) <$> pure x <*> find (hammingSimilar 1 x) xs
    go _ = Nothing

partTwo :: [BoxID] -> Maybe String
partTwo boxIDs = uncurry commonElems =<< correctBoxIDs boxIDs

main :: IO ()
main = do
  input <- parseInput (boxID `sepEndBy` newline) $(inputFilePath)
  putStr "Part One: "
  print (partOne input)
  putStr "Part Two: "
  putStrLn (maybe "failed!" id (partTwo input))
