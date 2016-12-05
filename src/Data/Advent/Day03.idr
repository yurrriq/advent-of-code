-- --------------------------------------------------------------- [ Day03.idr ]
-- Module      : Data.Advent.Day03
-- Description : My solution to the Day 3 puzzle of the 2016 Advent of Code.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : http://adventofcode.com/2016/day/3
-- --------------------------------------------------------------------- [ EOH ]
||| Day 3: Squares with Three Sides
module Data.Advent.Day03

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings
import public Lightyear.StringFile

%access export

-- ----------------------------------------------------------------- [ Parsers ]

triangle : Parser (Vect 3 Integer)
triangle = ntimes 3 (spaces *> integer) <?> "three side lengths"

triangles : Parser (List (Vect 3 Integer))
triangles = some triangle <?> "a list of triangle side lengths"

-- ------------------------------------------------------------------- [ Logic ]

isEquilateral : Vect 3 Integer -> Bool
isEquilateral [x,y,z] = x == y && y == z

isLogical : Vect 3 Integer -> Bool
isLogical [x,y,z] = x < y + z && y < z + x && z < x + y

isPossible : Vect 3 Integer -> Bool
isPossible sides = isEquilateral sides || isLogical sides

countPossible : List (Vect 3 Integer) -> Integer
countPossible = foldl go 0
  where
    go : Integer -> Vect 3 Integer -> Integer
    go n sides = if isPossible sides then n + 1 else n

-- ---------------------------------------------------------------- [ Part One ]

namespace PartOne

    main : IO ()
    main =
        do Right input <- run $ parseFile rErr pErr triangles "input/day03.txt"
             | Left err => putStrLn err
           printLn $ countPossible input
      where
        rErr : String -> FileError -> String
        rErr = const show
        pErr : String -> String -> String
        pErr = const id

-- -------------------------------------------------------------------- [ Main ]

namespace Main

    main : IO ()
    main = putStr "Part One: " *> PartOne.main

-- --------------------------------------------------------------------- [ EOF ]
