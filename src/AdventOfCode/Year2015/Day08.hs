module AdventOfCode.Year2015.Day08 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (cata)
import Data.Monoid (Sum (..))
import Text.Trifecta hiding (charLiteral)

main :: IO ()
main = $(defaultMain)

partOne :: [[String]] -> Int
partOne = getSum . uncurry subtract . cata encodeAlg
  where
    encodeAlg (Cons strings acc) = acc <> (lengthDecoded &&& lengthEncoded) strings
    encodeAlg Nil = (0, 0)

partTwo :: [[String]] -> Int
partTwo = getSum . uncurry subtract . cata reEncodeAlg
  where
    reEncodeAlg (Cons strings acc) = acc <> (lengthEncoded &&& lengthReEncoded) strings
    reEncodeAlg Nil = (0, 0)

getInput :: IO [[String]]
getInput = parseInput (some charLiteral `sepEndBy` newline) $(inputFilePath)

charLiteral :: Parser String
charLiteral =
  try hexChar
    <|> try (string "\\\"")
    <|> try (string "\\\\")
    <|> try (string "\"")
    <|> try (some lower)

hexChar :: Parser String
hexChar = (<>) <$> string "\\x" <*> count 2 hexDigit

lengthDecoded :: [String] -> Sum Int
lengthDecoded = Sum . sum . map go
  where
    go "\"" = 0
    go "\\\\" = 1
    go ('\\' : 'x' : _ : _) = 1
    go (_ : cs) = 1 + go cs
    go [] = 0

lengthEncoded :: [String] -> Sum Int
lengthEncoded = Sum . sum . map go
  where
    go "\"" = 1
    go "\\\\" = 2
    go "\\\"" = 2
    go ('\\' : 'x' : _ : _) = 4
    go s = length s

lengthReEncoded :: [String] -> Sum Int
lengthReEncoded = Sum . sum . map go
  where
    go "\"" = 3
    go "\\\\" = 4
    go "\\\"" = 4
    go ('\\' : 'x' : _ : _) = 5
    go s = length s
