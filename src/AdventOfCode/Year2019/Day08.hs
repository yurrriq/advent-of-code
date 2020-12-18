module AdventOfCode.Year2019.Day08
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Applicative ((<|>))
import Data.Function (on)
import Data.List (minimumBy)
import Text.Trifecta ((<?>), Parser, char, count, some)

data Pixel
  = Black
  | White
  | Transparent
  deriving (Enum, Eq)

instance Show Pixel where
  show Black = " "
  show White = "#"
  show Transparent = "."

type Image = [Layer]

type Layer = [Row]

type Row = [Pixel]

main :: IO ()
main =
  do
    putStrLn "[2019] Day 8: Space Image Format"
    layers <- parseInput (image 25 6) $(inputFilePath)
    putStr "Part One: "
    print (partOne layers)
    putStrLn "Part Two: "
    putStrLn (partTwo layers)

partOne :: Image -> Int
partOne layers = numberOf White layer * numberOf Transparent layer
  where
    layer = minimumBy (compare `on` numberOf Black) layers
    numberOf :: Eq a => a -> [[a]] -> Int
    numberOf x = sum . fmap (length . filter (== x))

partTwo :: Image -> String
partTwo layers =
  unlines . map (concatMap show) $
    foldl decodeLayer (transparentLayer 25 6) layers
  where
    decodeLayer :: Layer -> Layer -> Layer
    decodeLayer = zipWith (zipWith decodePixel)
    decodePixel :: Pixel -> Pixel -> Pixel
    decodePixel Transparent below = below
    decodePixel above _ = above

image :: Int -> Int -> Parser Image
image width height = some layer
  where
    layer :: Parser Layer
    layer = count height row
    row :: Parser Row
    row = count width pixel

pixel :: Parser Pixel
pixel =
  (char '0' *> pure Black <?> "A black pixel")
    <|> (char '1' *> pure White <?> "A white pixel")
    <|> (char '2' *> pure Transparent <?> "A transparent pixel")

transparentLayer :: Int -> Int -> Layer
transparentLayer width height = replicate height (replicate width Transparent)
