module Main (main) where

import Data.Advent.Day05

main :: IO ()
main = do doorID <- head . words <$> readFile "input/day05.txt"
          putStr "Part One: "
          putStrLn (partOne doorID)
          putStr "Part Two: "
          putStrLn (partTwo doorID)
