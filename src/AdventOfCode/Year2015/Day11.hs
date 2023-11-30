{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Year2015.Day11 where

import AdventOfCode.TH
import Control.Arrow ((***), (>>>))
import Control.Monad ((>=>))
import Data.List (find, group, isPrefixOf, nub)

main :: IO ()
main = $(defaultMain)

getInput :: IO String
getInput = pure "cqjxjnds"

partOne :: String -> Maybe String
partOne = findNextPassword

partTwo :: String -> Maybe String
partTwo = partOne >=> findNextPassword

findNextPassword :: String -> Maybe String
findNextPassword =
  fmap reverse
    . find isValidPasswordR
    . tail
    . iterate stepPasswordR
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
countUniquePairs = length . nub . map head . filter ((>= 2) . length) . group

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
