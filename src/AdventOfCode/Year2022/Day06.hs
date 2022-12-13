{-# LANGUAGE DataKinds #-}

module AdventOfCode.Year2022.Day06 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative (liftA2)
import Data.Char (isAlpha)
import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as VS
import Text.Trifecta (Parser, letter, manyTill, satisfy, try)

main :: IO ()
main = $(defaultMain)

partOne :: Int -> Int
partOne = id

partTwo :: Int -> Int
partTwo = undefined

getInput :: IO Int
getInput = parseInput datastream $(inputFilePath)

datastream :: Parser Int
datastream = (4 +) . length <$> manyTill letter (try sop)

sop :: Parser (Vector 4 Char)
sop =
  do
    a <- letter
    b <- satisfy (isAlpha <&&> (/= a))
    c <- satisfy (isAlpha <&&> (`notElem` [a, b]))
    d <- satisfy (isAlpha <&&> (`notElem` [a, b, c]))
    case VS.fromList [a, b, c, d] of
      Just xs -> pure xs
      Nothing -> error "Can't make vector"

examples :: IO [Int]
examples =
  mapM
    (parseString datastream)
    [ "bvwbjplbgvbhsrlpgdmjqwftvncz",
      "nppdvjthqldpwncqszvftbrmjlhg",
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    ]

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

infixr 3 <&&>
