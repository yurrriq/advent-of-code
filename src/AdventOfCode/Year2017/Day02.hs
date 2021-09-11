module AdventOfCode.Year2017.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (holes)
import Control.Arrow ((&&&))
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Text.Trifecta (digit, newline, sepBy, some, tab)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: " *> print (partOne input)
    putStr "Part Two: " *> print (partTwo input)

getInput :: IO [[Int]]
getInput = parseInput (some (row <* newline)) $(inputFilePath)
  where
    row = int `sepBy` tab
    int = read <$> some digit

partOne :: [[Int]] -> Int
partOne = sum . map (uncurry (-) . (maximum &&& minimum))

partTwo :: [[Int]] -> Int
partTwo = sum . mapMaybe handleRow
  where
    handleRow :: [Int] -> Maybe Int
    handleRow xs =
      listToMaybe . catMaybes $
        do
          (x, ys) <- holes xs
          y <- ys
          case (quotRem x y, quotRem y x) of
            ((q, 0), _) -> pure $ Just q
            (_, (q, 0)) -> pure $ Just q
            _ -> pure Nothing
