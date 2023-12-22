module AdventOfCode.Year2023.Day06 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (count)
import Data.Composition ((.:))
import Data.FastDigits (digits, undigits)
import Data.List.Extra (productOn')
import Text.Trifecta hiding (count, parseString)

newtype Race = Race {unRace :: (Int, Int)}
  deriving (Eq, Show)

main :: IO ()
main = $(defaultMain)

partOne :: [Race] -> Int
partOne = productOn' waysToWin

partTwo :: [Race] -> Int
partTwo = waysToWin . convert
  where
    convert races =
      let (timeDigits, distanceDigits) = foldl go ([], []) races
       in Race
            ( fromInteger (undigits (10 :: Int) (concat timeDigits)),
              fromInteger (undigits (10 :: Int) (concat distanceDigits))
            )
      where
        go (timeDigits, distanceDigits) (Race (time, distance)) =
          ( digits 10 (toInteger time) : timeDigits,
            digits 10 (toInteger distance) : distanceDigits
          )

waysToWin :: Race -> Int
waysToWin (Race (time, distance)) =
  count (\speed -> (time - speed) * speed > distance) [1 .. time - 1]

getInput :: IO [Race]
getInput = parseInput document $(inputFilePath)

document :: Parser [Race]
document =
  zipWith (Race .: (,))
    <$> (symbol "Time:" *> some posInt)
    <*> (symbol "Distance:" *> some posInt)

posInt :: Parser Int
posInt = fromInteger <$> natural

getExample :: IO [Race]
getExample = parseString document example

example :: String
example =
  "Time:      7  15   30\n\
  \Distance:  9  40  200\n"
