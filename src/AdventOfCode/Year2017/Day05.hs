{-# LANGUAGE StrictData #-}

module AdventOfCode.Year2017.Day05 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMainMaybe, inputFilePath)
import Data.List (unfoldr)
import Data.Tuple.Extra (dupe)
import Text.Trifecta (integer, some)

data ListZipper a = ListZipper [a] a [a]
  deriving (Show)

main :: IO ()
main = $(defaultMainMaybe)

getInput :: IO [Int]
getInput = parseInput (some (fromInteger <$> integer)) $(inputFilePath)

partOne :: [Int] -> Maybe Int
partOne = solve (+ 1)

partTwo :: [Int] -> Maybe Int
partTwo = solve updateOffset
  where
    updateOffset offset
      | offset >= 3 = offset - 1
      | otherwise = offset + 1

solve :: (Int -> Int) -> [Int] -> Maybe Int
solve updateOffset = fmap (length . iterateMaybe (step updateOffset)) . zipper

example :: [Int]
example = [0, 3, 0, 1, -3]

step :: (Int -> Int) -> ListZipper Int -> Maybe (ListZipper Int)
step f (ListZipper ls x rs) = move x (ListZipper ls (f x) rs)

move :: Int -> ListZipper a -> Maybe (ListZipper a)
move n = case compare n 0 of
  LT -> (!! abs n) . iterate (moveLeft =<<) . Just
  EQ -> Just
  GT -> (!! n) . iterate (moveRight =<<) . Just

moveRight :: ListZipper a -> Maybe (ListZipper a)
moveRight (ListZipper _ _ []) = Nothing
moveRight (ListZipper ls x (r : rs)) = Just (ListZipper (x : ls) r rs)

moveLeft :: ListZipper a -> Maybe (ListZipper a)
moveLeft (ListZipper [] _ _) = Nothing
moveLeft (ListZipper (l : ls) x rs) = Just (ListZipper ls l (x : rs))

zipper :: [a] -> Maybe (ListZipper a)
zipper [] = Nothing
zipper (x : rs) = Just (ListZipper [] x rs)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : unfoldr (fmap dupe . f) x
