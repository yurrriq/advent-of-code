module Main where


import           Day05 (partOne, partTwo)


main :: IO ()
main = do input <- readFile "./input/day05.txt"
          putStr "Part One: "
          print (partOne input)
          putStr "Part Two: "
          print (partTwo input)
