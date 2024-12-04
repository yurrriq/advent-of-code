module AdventOfCode.Year2024.Day03 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Either (rights)
import Data.List.Extra (sumOn')
import Text.Parser.LookAhead (lookAhead)
import Text.Trifecta
  ( anyChar,
    comma,
    decimal,
    eof,
    manyTill,
    parens,
    some,
    string,
    try,
  )

main :: IO ()
main = $(defaultMain)

partOne :: [Either Bool (Integer, Integer)] -> Integer
partOne = sumOn' (uncurry (*)) . rights

partTwo :: [Either Bool (Integer, Integer)] -> Integer
partTwo = go
  where
    go [] = 0
    go (Left False : muls) = go (dropWhile (/= Left True) muls)
    go muls =
      let (xs, ys) = span (/= Left False) muls
       in partOne xs + go ys

getInput :: IO [Either Bool (Integer, Integer)]
getInput = parseInput (some go) $(inputFilePath)
  where
    go =
      skipNoise
        *> ( (Left <$> toggle)
               <|> (Right <$> multiplication)
           )
    skipNoise =
      do
        void . manyTill anyChar $
          lookAhead
            ( void multiplication
                <|> void toggle
                <|> eof
            )
    toggle =
      try $
        (True <$ string "do()")
          <|> (False <$ string "don't()")
    multiplication =
      try $
        string "mul"
          *> parens ((,) <$> (decimal <* comma) <*> decimal)
