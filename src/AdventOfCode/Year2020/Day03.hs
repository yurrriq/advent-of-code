module AdventOfCode.Year2020.Day03
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (count)
import Control.Applicative ((<|>))
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Text.Trifecta (Parser, char, newline, sepEndBy, some)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne input
    putStr "Part Two: "
    print $ partTwo input

getInput :: IO (Vector (Vector Bool))
getInput = parseInput tobogganMap $(inputFilePath)

partOne :: Vector (Vector Bool) -> Int
partOne = slide (3, 1)

partTwo :: Vector (Vector Bool) -> Int
partTwo theMap =
  product $
    map (flip slide theMap) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

tobogganMap :: Parser (Vector (Vector Bool))
tobogganMap = V.fromList . map V.fromList <$> some square `sepEndBy` newline

square :: Parser Bool
square =
  char '.' *> pure False
    <|> char '#' *> pure True

slide :: (Int, Int) -> Vector (Vector Bool) -> Int
slide (right, down) theMap = count id (map go [0 .. (V.length theMap - 1) `div` down])
  where
    go k = (theMap ! (k * down)) ! (k * right `mod` width)
    width = length (V.head theMap)
