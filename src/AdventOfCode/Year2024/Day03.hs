module AdventOfCode.Year2024.Day03 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Monad (void)
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

partOne :: [(Integer, Integer)] -> Integer
partOne = sumOn' (uncurry (*))

partTwo :: [(Integer, Integer)] -> Integer
partTwo = undefined

getInput :: IO [(Integer, Integer)]
getInput = parseInput (some (skipNoise *> multiplication)) $(inputFilePath)
  where
    multiplication =
      try $ string "mul" *> parens ((,) <$> (decimal <* comma) <*> decimal)
    skipNoise =
      do
        void . manyTill anyChar $
          lookAhead
            ( void multiplication
                <|> eof
            )
