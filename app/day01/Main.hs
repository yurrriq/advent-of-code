module Main where

import qualified Data.ByteString as BS
import Day01 (partOne, partTwo)

main :: IO ()
main = do
  input <- BS.readFile "./input/day01.txt"
  putStr "Part One: "
  putStrLn $ maybe "failed!" show (partOne input)
  putStr "Part Two: "
  putStrLn $ maybe "failed!" show (partTwo input)
