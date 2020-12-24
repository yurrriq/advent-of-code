module AdventOfCode.Year2020.Day04
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Applicative ((<|>), some)
import Control.Arrow (first)
import Control.Monad (foldM)
import Data.Bool (bool)
import Data.Char (isDigit, isHexDigit)
import Data.Ix (inRange)
import Data.Map (Map)
import qualified Data.Map as Map
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

getInput :: IO [[(String, Either Int String)]]
getInput = parseInput (passport `sepEndBy` newline) $(inputFilePath)

partOne :: [[(String, Either Int String)]] -> Int
partOne =
  length
    . filter (flip all requiredFields . flip Map.member)
    . map Map.fromList

partTwo :: [[(String, Either Int String)]] -> Int
partTwo =
  length
    . filter (flip all requiredFields . flip Map.member)
    . mapMaybe validPassport

validPassport :: [(String, Either Int String)] -> Maybe (Map String (Either Int String))
validPassport fs = foldM go Map.empty fs
  where
    go m f = mappend m . uncurry Map.singleton <$> validateField f

validateField :: (String, Either Int String) -> Maybe (String, Either Int String)
validateField x@("byr", Left year) =
  bool Nothing (Just x) $
    inRange (1920, 2002) year
validateField x@("iyr", Left year) =
  bool Nothing (Just x) $
    inRange (2010, 2020) year
validateField x@("eyr", Left year) =
  bool Nothing (Just x) $
    inRange (2020, 2030) year
validateField x@("hgt", Right height) =
  case first (read :: String -> Int) (span isDigit height) of
    (n, "cm") ->
      bool Nothing (Just x) $
        inRange (150, 193) n
    (n, "in") ->
      bool Nothing (Just x) $
        inRange (59, 76) n
    _ -> Nothing
validateField x@("hcl", Right ('#' : cs)) =
  bool Nothing (Just x) $
    6 == length (filter isHexDigit cs)
validateField x@("ecl", Right color) =
  bool Nothing (Just x) $
    color `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validateField x@("pid", Right pid) =
  bool Nothing (Just x) $
    9 == length (filter isDigit pid)
validateField x@("cid", _) = Just x
validateField _ = Nothing

passport :: Parser [(String, Either Int String)]
passport = concat <$> some field `sepEndBy` (try newline <|> try space)

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
