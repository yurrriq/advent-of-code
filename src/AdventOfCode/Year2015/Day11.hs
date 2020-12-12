{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AdventOfCode.Year2015.Day11
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import Control.Monad ((>=>))
import Data.Char (ord)
import Data.List (find, group, isPrefixOf, nub)
import Data.Maybe (fromMaybe)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    putStrLn $ fromMaybe "failed!" $ partOne input
    putStr "Part Two: "
    putStrLn $ fromMaybe "failed!" $ partTwo input

getInput :: IO String
getInput = pure "cqjxjnds"

partOne :: String -> Maybe String
partOne = findNextPassword

partTwo :: String -> Maybe String
partTwo = partOne >=> findNextPassword

isValidPassword :: String -> Bool
isValidPassword str =
  not (isConfusing str)
    && countUniquePairs str >= 2
    && hasStraight 3 str

isConfusing :: String -> Bool
isConfusing = any (flip elem "iol")

hasStraight :: Int -> String -> Bool
hasStraight n = go
  where
    go (c : cs) = (maybe False (flip isPrefixOf cs) (buildStraight (n - 1) c)) || go cs
    go [] = False

-- TODO: s/Char/Bounded a/
buildStraight :: Int -> Char -> Maybe String
buildStraight n c
  | (ord 'z' - ord c) >= n - 1 = Just $ take n (iterate succ c)
  | otherwise = Nothing

countUniquePairs :: String -> Int
countUniquePairs = length . nub . map head . filter ((>= 2) . length) . group

findNextPassword :: String -> Maybe String
findNextPassword = find isValidPassword . tail . iterate stepPassword
  where
    stepPassword = reverse . go . reverse
    go ('z' : cs) = 'a' : go cs
    go (c : cs) = nextChar c : cs
    go [] = []

-- TODO: clamp
nextChar :: Char -> Char
nextChar 'z' = 'a'
nextChar 'h' = 'j'
nextChar 'n' = 'p'
nextChar 'k' = 'm'
nextChar c = succ c