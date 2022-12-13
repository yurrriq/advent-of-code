module AdventOfCode.Year2022.Day06 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (inputFilePath)
import Control.Applicative (liftA2)
import Data.Char (isAlpha)
import Text.Trifecta (Parser, letter, manyTill, satisfy, some, try)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    n <- partOne input
    print n
    putStr "Part Two: "
    m <- (n +) <$> partTwo (drop n input)
    print m

partOne :: String -> IO Int
partOne = parseString (findSop 4)

partTwo :: String -> IO Int
partTwo = parseString (findSop 14)

getInput :: IO String
getInput = parseInput (some letter) $(inputFilePath)

findSop :: Int -> Parser Int
findSop n = (n +) . length <$> manyTill letter (try (sop n))

sop :: Int -> Parser [Char]
sop = go []
  where
    go seen 0 = pure (reverse seen)
    go seen n =
      do
        c <- satisfy (isAlpha <&&> (`notElem` seen))
        go (c : seen) (n - 1)

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

infixr 3 <&&>
