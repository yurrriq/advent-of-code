-- --------------------------------------------------------------- [ Day06.idr ]
-- Module      : Data.Advent.Day06
-- Description : My solution to the Day 46 puzzle of the 2016 Advent of Code.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : http://adventofcode.com/2016/day/6
-- --------------------------------------------------------------------- [ EOH ]
||| Day 6: Signals and Noise
module Data.Advent.Day06

import public Data.SortedMap

%access export

private
inc : k -> SortedMap k Nat -> SortedMap k Nat
inc k m = case lookup k m of
               Nothing => insert k 1 m
               Just v  => insert k (S v) $ delete k m

main' : (String -> String) -> IO ()
main' f = either printLn (putStrLn . f) !(readFile "input/day06.txt")

namespace PartOne

    decode : String -> String
    decode = pack . map (fst . foldr1 go . toList) .
             map (foldr inc empty) . transpose .
             map unpack . lines
      where
        go : (Char, Nat) -> (Char, Nat) -> (Char, Nat)
        go elem@(c1,n1) acc@(c2,n2) = if n1 > n2 then elem else acc

    main : IO ()
    main = main' decode

namespace Main

    main : IO ()
    main = putStr "Part One: " *> PartOne.main

-- --------------------------------------------------------------------- [ EOF ]
