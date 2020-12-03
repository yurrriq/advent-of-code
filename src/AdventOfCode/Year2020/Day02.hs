module AdventOfCode.Year2020.Day02
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Monad (void)
import Data.Ix (inRange)
import Text.Trifecta (Parser, char, lower, natural, newline, sepEndBy, some, symbol)

infixr 2 `xor`

data PasswordPolicy
  = PP (Int, Int) Char
  deriving (Eq, Show)

main :: IO ()
main =
  do
    validations <- parseInput (passwordValidation `sepEndBy` newline) $(inputFilePath)
    putStr "Part One: "
    print $ partOne validations
    putStr "Part Two: "
    print $ partTwo validations

partOne :: [(PasswordPolicy, String)] -> Int
partOne = count (uncurry isValid)
  where
    isValid (PP allowedOccurrences letter) password =
      inRange allowedOccurrences (count (== letter) password)

partTwo :: [(PasswordPolicy, String)] -> Int
partTwo = count (uncurry isValid)
  where
    isValid (PP (n, m) letter) password =
      password !! pred n == letter `xor` password !! pred m == letter

passwordValidation :: Parser (PasswordPolicy, String)
passwordValidation = (,) <$> passwordPolicy <*> some lower

passwordPolicy :: Parser PasswordPolicy
passwordPolicy =
  do
    range <- (,) <$> (posInt <* char '-') <*> posInt
    letter <- lower
    void $ symbol ":"
    pure (PP range letter)

posInt :: Parser Int
posInt = fromInteger <$> natural

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)
