module AdventOfCode.Year2017.Day01
  ( main,
    getInput,
    digits,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (snoc)
import Control.Arrow ((&&&), (<<<), (>>>))
import Data.Char (digitToInt)
import Data.List (group)
import qualified Data.Vector as V
import Text.Trifecta (Parser, digit, some)

getInput :: IO [Int]
getInput = parseInput p $(inputFilePath)
  where
    p = (uncurry snoc <<< (id &&& head)) <$> digits

digits :: Parser [Int]
digits = map digitToInt <$> some digit

partOne :: [Int] -> Int
partOne =
  sum
    . map ((head &&& (subtract 1 . length)) >>> uncurry (*))
    . filter ((> 1) . length)
    . group

partTwo :: [Int] -> Int
partTwo xs = V.ifoldl' go 0 v
  where
    go z i x
      | x == v V.! ((i + k) `mod` n) = z + x
      | otherwise = z
    v = V.fromList xs
    k = n `div` 2
    n = V.length v
