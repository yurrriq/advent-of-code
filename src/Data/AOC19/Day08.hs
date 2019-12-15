module Data.AOC19.Day08 where

import           Data.Function      (on)
import           Data.List          (sortBy)
import           System.Environment (getArgs)
import           Text.Trifecta      (Parser, count, digit, many, parseFromFile)


type Row = [Char]


type Layer = [Row]


main :: IO ()
main =
    do putStr "Part One: "
       partOne =<< getInputFilename


partOne :: FilePath -> IO ()
partOne fname =
    do Just layers <- parseFromFile (image 25 6) fname
       let layer = head $ sortBy (compare `on` numberOf '0') layers
       let ones = numberOf '1' layer
       let twos = numberOf '2' layer
       print $ ones * twos
  where
    numberOf x = sum . fmap (length . filter (== x))


image :: Int -> Int -> Parser [Layer]
image width height = many layer
  where
    layer = count height row
    row = count width digit


getInputFilename :: IO FilePath
getInputFilename =
  do args <- getArgs
     case args of
       [fname] -> pure fname
       []      -> error "Must specify input filename"
       _       -> error "Too many args"
