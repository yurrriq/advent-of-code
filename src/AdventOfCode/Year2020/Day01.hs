module AdventOfCode.Year2020.Day01
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Data.Group (Group, invert)
import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid (Sum (..), getSum)
import Text.Trifecta (Parser, natural, some)

main :: IO ()
main =
  do
    entries <- parseInput (some posInt) $(inputFilePath)
    putStr "Part One: "
    print $ partOne entries
    putStr "Part Two: "
    print $ partTwo entries

partOne :: [Int] -> Maybe Int
partOne = fmap (getSum . product) . knapsack 2 2020 . map Sum

partTwo :: [Int] -> Maybe Int
partTwo = fmap (getSum . product) . knapsack 3 2020 . map Sum

knapsack :: (Eq a, Ord a, Group a) => Int -> a -> [a] -> Maybe [a]
knapsack 0 _ _ = Nothing
knapsack 1 goal xs = pure <$> find (== goal) xs
knapsack n goal xs =
  listToMaybe . flip mapMaybe xs $ \x ->
    (x :) <$> knapsack (pred n) (goal <> invert x) (filter (> x) xs)

posInt :: Parser Int
posInt = fromInteger <$> natural
