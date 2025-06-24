module AdventOfCode.Year2020.Day04
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Applicative (some, (<|>))
import Control.Arrow (first)
import Data.Bool (bool)
import Data.Char (isDigit, isHexDigit)
import Data.Ix (inRange)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Text.Trifecta
  ( Parser,
    alphaNum,
    char,
    choice,
    digit,
    lower,
    newline,
    notFollowedBy,
    oneOf,
    sepEndBy,
    space,
    string,
    try,
  )

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print (partOne input)
    putStr "Part Two: "
    print (partTwo input)

getInput :: IO [Passport]
getInput = parseInput (passport `sepEndBy` newline) $(inputFilePath)

partOne :: [Passport] -> Int
partOne =
  length
    . filter (flip all requiredFields . flip Map.member)

partTwo :: [Passport] -> Int
partTwo =
  length
    . filter (flip all requiredFields . flip Map.member)
    . mapMaybe validPassport

validPassport :: Passport -> Maybe Passport
validPassport = Map.foldMapWithKey validateField

validateField :: String -> Either Int String -> Maybe Passport
validateField k v =
  case (k, v) of
    ("byr", Left year) ->
      go (inRange (1920, 2002) year)
    ("iyr", Left year) ->
      go (inRange (2010, 2020) year)
    ("eyr", Left year) ->
      go (inRange (2020, 2030) year)
    ("hgt", Right height) ->
      case first (read :: String -> Int) (span isDigit height) of
        (n, "cm") ->
          go (inRange (150, 193) n)
        (n, "in") ->
          go (inRange (59, 76) n)
        _ -> Nothing
    ("hcl", Right ('#' : cs)) ->
      go (6 == length cs && all isHexDigit cs)
    ("ecl", Right color) ->
      go (color `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    ("pid", Left pid) ->
      go (9 == length (show pid))
    ("pid", Right pid) ->
      go (9 == length pid && all isDigit pid)
    _ -> Nothing
  where
    go = bool Nothing success
    success = Just (Map.singleton k v)

type Passport = Map String (Either Int String)

passport :: Parser Passport
passport = Map.fromList . concat <$> some field `sepEndBy` (try newline <|> try space)

field :: Parser (String, Either Int String)
field =
  do
    name <- choice (string <$> allFields) <* char ':'
    value <- try (Left <$> numericValue) <|> (Right <$> stringValue)
    pure (name, value)

allFields, requiredFields :: [String]
allFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
requiredFields = init allFields

numericValue :: Parser Int
numericValue = posInt' <* notFollowedBy lower

stringValue :: Parser String
stringValue = some (char '#' <|> alphaNum)

posInt' :: Parser Int
posInt' = read <$> ((:) <$> oneOf "123456789" <*> some digit)
