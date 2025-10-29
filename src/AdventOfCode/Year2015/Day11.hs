{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Year2015.Day11 where

import AdventOfCode.TH
import Control.Arrow ((***), (>>>))
import Data.List.Extra (isPrefixOf, nubOrd)
import Data.List.Infinite qualified as Infinite
import Data.List.NonEmpty qualified as NE

main :: IO ()
main = $(defaultMain)

getInput :: IO String
getInput = pure "cqjxjnds"

partOne :: String -> String
partOne = findNextPassword

partTwo :: String -> String
partTwo = partOne >>> findNextPassword

findNextPassword :: String -> String
findNextPassword =
  reverse
    . Infinite.find isValidPasswordR
    . Infinite.tail
    . Infinite.iterate stepPasswordR
    . reverse

stepPasswordR :: String -> String
stepPasswordR =
  span (== 'z')
    >>> (map (const 'a') *** mapHead nextChar)
    >>> uncurry (++)

nextChar :: Char -> Char
nextChar 'h' = 'j'
nextChar 'k' = 'm'
nextChar 'n' = 'p'
nextChar 'z' = 'a'
nextChar c = succ c

mapHead :: (a -> a) -> [a] -> [a]
mapHead f = \case
  [] -> []
  x : xs -> f x : xs

isValidPasswordR :: String -> Bool
isValidPasswordR str =
  not (isConfusing str)
    && countUniquePairs str >= 2
    && hasStraightR 3 str

isConfusing :: String -> Bool
isConfusing = any (`elem` "iol")

countUniquePairs :: String -> Int
countUniquePairs = length . nubOrd . map NE.head . filter ((>= 2) . NE.length) . NE.group

hasStraightR :: Int -> String -> Bool
hasStraightR n = go
  where
    go s@(c : cs) = maybe False (`isPrefixOf` s) (buildStraightR n c) || go cs
    go [] = False

buildStraightR :: Int -> Char -> Maybe String
buildStraightR 0 _ = Just []
buildStraightR 1 c = Just [c]
buildStraightR _ 'a' = Nothing
buildStraightR n c = (c :) <$> buildStraightR (n - 1) (pred c)
