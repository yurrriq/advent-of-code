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

-- ----------------------------------------------------------- [ Generic Logic ]

main' : (String -> String) -> IO ()
main' f = either printLn (putStrLn . f) !(readFile "input/day06.txt")

decode' : (f : Nat -> Nat -> Bool) -> String -> String
decode' f = pack . map (fst . foldr1 go . frequencies') . transpose .
            map unpack . lines
  where
    go : (Char, Nat) -> (Char, Nat) -> (Char, Nat)
    go elem@(_,m) acc@(_,n) = if m `f` n then elem else acc
    frequencies' : Ord a => List a -> List (a, Nat)
    frequencies' = toList . foldr inc empty
      where
        inc : k -> SortedMap k Nat -> SortedMap k Nat
        inc k m = case lookup k m of
                       Nothing => insert k 1 m
                       Just v  => insert k (S v) $ delete k m

namespace PartOne

    decode : String -> String
    decode = decode' (>)

    main : IO ()
    main = main' decode

namespace Main

    main : IO ()
    main = putStr "Part One: " *> PartOne.main

-- --------------------------------------------------------------------- [ EOF ]
