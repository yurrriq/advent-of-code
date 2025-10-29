module AdventOfCode.Year2023.Day01 where

import AdventOfCode.Input (parseString, rawInput)
import AdventOfCode.TH (defaultMainIO, inputFilePath)
import AdventOfCode.Util ((<.>))
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (digitToInt)
import Data.List.NonEmpty qualified as NE
import Text.Parser.LookAhead (lookAhead)
import Text.Trifecta hiding (digit, parseString)
import Text.Trifecta qualified as Trifecta

main :: IO ()
main = $(defaultMainIO)

getInput :: IO String
getInput = rawInput $(inputFilePath)

partOne :: String -> IO Int
partOne = calibrate digit

partTwo :: String -> IO Int
partTwo = calibrate digitOrSpelledDigit

calibrate :: Parser Int -> String -> IO Int
calibrate p = sum <.> parseString (calibrationDocument p)

calibrationDocument :: Parser Int -> Parser [Int]
calibrationDocument p = (calibrationValue <$> NE.some1 p) `sepEndBy` newline
  where
    calibrationValue = (+) <$> (10 *) . NE.head <*> NE.last

digit :: Parser Int
digit = digitToInt <$> Trifecta.digit `surroundedBy` many letter

digitOrSpelledDigit :: Parser Int
digitOrSpelledDigit =
  do
    skipLetters
    d <- (digitToInt <$> Trifecta.digit) <|> (lookAhead spelledDigit <* letter)
    skipLetters
    pure d
  where
    skipLetters =
      void . manyTill letter $
        lookAhead
          ( void spelledDigit
              <|> void Trifecta.digit
              <|> void newline
              <|> eof
          )

spelledDigit :: Parser Int
spelledDigit =
  (1 <$ string "one")
    <|> (2 <$ string "two")
    <|> (3 <$ string "three")
    <|> (4 <$ string "four")
    <|> (5 <$ string "five")
    <|> (6 <$ string "six")
    <|> (7 <$ string "seven")
    <|> (8 <$ string "eight")
    <|> (9 <$ string "nine")

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
