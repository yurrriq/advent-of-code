module AdventOfCode.Year2023.Day01 where

import AdventOfCode.Input -- (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Paths_advent_of_code (getDataFileName)
import Text.Parser.LookAhead (lookAhead)
import Text.Trifecta hiding (parseString)

-- main :: IO ()
-- main = $(defaultMain)

-- partOne :: [String] -> Int
-- partOne = undefined
--   where

-- partTwo :: [String] -> Int
-- partTwo = undefined

getInput :: IO [Int]
getInput = parseInput calibrationDocument $(inputFilePath)

-- lines <$> (readFile =<< getDataFileName $(inputFilePath))

calibrationDocument :: Parser [Int]
calibrationDocument = calibrationValue `sepEndBy` newline

calibrationValue :: Parser Int
calibrationValue = some partOneDigit <&> mkCalibrationValue

mkCalibrationValue :: [Char] -> Int
mkCalibrationValue ds = 10 * digitToInt (head ds) + digitToInt (last ds)

partOneDigit :: Parser Char
partOneDigit = (many letter *> digit) <* many letter

partTwoDigit :: Parser Char
partTwoDigit =
  do
    skipLetters
    d <- digit <|> spelledDigit
    skipLetters
    pure d
  where
    skipLetters =
      void . manyTill letter $
        lookAhead
          ( void spelledDigit
              <|> void digit
              <|> void newline
              <|> eof
          )

spelledDigit :: Parser Char
spelledDigit =
  ('1' <$ string "one")
    <|> ('2' <$ string "two")
    <|> ('3' <$ string "three")
    <|> ('4' <$ string "four")
    <|> ('5' <$ string "five")
    <|> ('6' <$ string "six")
    <|> ('7' <$ string "seven")
    <|> ('8' <$ string "eight")
    <|> ('9' <$ string "nine")

examples :: [String]
examples =
  unlines
    <$> [ [ "1abc2",
            "pqr3stu8vwx",
            "a1b2c3d4e5f",
            "treb7uchet"
          ],
          [ "two1nine",
            "eightwothree",
            "abcone2threexyz",
            "xtwone3four",
            "4nineeightseven2",
            "zoneight234",
            "7pqrstsixteen"
          ]
        ]
