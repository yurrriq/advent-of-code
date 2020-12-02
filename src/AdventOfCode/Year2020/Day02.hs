module AdventOfCode.Year2020.Day02
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Util (parseInput)
import Control.Monad (void)
import Data.Ix (inRange)
import Text.Trifecta (Parser, char, lower, natural, newline, sepEndBy, some, symbol)

data PasswordPolicy
  = PP (Int, Int) Char
  deriving (Eq, Show)

main :: IO ()
main =
  do
    validations <- parseInput (((,) <$> passwordPolicy <*> some lower) `sepEndBy` newline) "input/2020/day02.txt"
    putStr "Part One: "
    print $ partOne validations
    putStr "Part Two: "
    print $ partTwo validations

partOne :: [(PasswordPolicy, String)] -> Int
partOne = length . filter (uncurry isValid)
  where
    isValid (PP allowedOccurrences letter) password =
      inRange allowedOccurrences (length (filter (== letter) password))

partTwo :: [(PasswordPolicy, String)] -> Int
partTwo = length . filter (uncurry isValid)
  where
    isValid (PP (n, m) letter) password =
      (password !! pred n == letter)
        `xor` (password !! pred m == letter)
    xor p q = (p || q) && not (p && q)

passwordPolicy :: Parser PasswordPolicy
passwordPolicy =
  do
    minOccurrences <- fromInteger <$> natural
    void $ char '-'
    maxOccurrences <- fromInteger <$> natural
    letter <- lower
    void $ symbol ":"
    pure (PP (minOccurrences, maxOccurrences) letter)
