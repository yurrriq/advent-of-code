-- --------------------------------------------------------------- [ Day03.idr ]
-- Module      : Data.Advent.Day03
-- Description : My solution to the Day 3 puzzle of the 2016 Advent of Code.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : http://adventofcode.com/2016/day/3
-- --------------------------------------------------------------------- [ EOH ]
||| Day 3: Squares with Three Sides
module Data.Advent.Day03

import public Data.Advent.Day

import Data.Vect

-- -------------------------------------------------------------- [ Data Types ]

%access export

||| A triangle, comprised of three side lengths, is equilateral or il/logical.
data Triangle : Type where

     ||| Given side lengths `a`, `b` and `c`, and proofs that `a = b`
     ||| and `b = c`, return a `Triangle.`
     Equilateral : (a, b, c : Integer) -> a = b -> b = c -> Triangle

     ||| Given side lengths `a`, `b` and `c`, and proofs that they satisfy the
     ||| triangle inequality theorem, return a `Triangle`.
     ||| @ p1 a proof that `a` is strictly less than `b + c`
     ||| @ p2 a proof that `b` is strictly less than `a + c`
     ||| @ p3 a proof that `c` is strictly less than `a + b`
     Logical : (a, b, c : Integer) ->
               (p1 : LT (fromIntegerNat a) (fromIntegerNat (b + c))) ->
               (p2 : LT (fromIntegerNat b) (fromIntegerNat (a + c))) ->
               (p3 : LT (fromIntegerNat c) (fromIntegerNat (a + b))) ->
               Triangle

     ||| Construct a triangle that is neither equilateral nor logical.
     Illogical : (a, b, c : Integer) -> Triangle

||| Given side lengths `a`, `b` and `c`, if they are all equal,
||| return `Just` a `Triangle`, otherwise `Nothing`.
equilateral : (a, b, c : Integer) -> Maybe Triangle
equilateral a b c with (decEq a b)
  equilateral _ _ _ | No _              = Nothing
  equilateral a b c | Yes ab with (decEq b c)
    equilateral _ _ _ | _ | No _        = Nothing
    equilateral a b c | Yes ab | Yes bc = Just (Equilateral a b c ab bc)

||| Given side lengths `a`, `b` and `c`, return `Just` a `Triangle` if they
||| satisfy the triangle inequality theorem, otherwise `Nothing`.
logical : (a, b, c : Integer) -> Maybe Triangle
logical a b c with (isLTE (S (fromIntegerNat a)) (fromIntegerNat (b + c)))
  logical _ _ _| No _                             = Nothing
  logical a b c | Yes abc with (isLTE (S (fromIntegerNat b))
                                      (fromIntegerNat (a + c)))
    logical _ _ _| _ | No _                       = Nothing
    logical a b c | Yes abc | Yes bac with (isLTE (S (fromIntegerNat c))
                                                  (fromIntegerNat (a + b)))
      logical _ _ _ | _       | _       | No _    = Nothing
      logical a b c | Yes abc | Yes bac | Yes cab =
          Just (Logical a b c abc bac cab)

namespace Triangle

  ||| Construct a `Triangle` from the given side lengths.
  triangle : (a, b, c : Integer) -> Triangle
  triangle a b c = fromMaybe (Illogical a b c) $
                   equilateral a b c <|> logical a b c

  ||| Construct a `Triangle` from a `Vect` of (three) side lengths (integers).
  fromVect : Vect 3 Integer -> Triangle
  fromVect [a,b,c] = triangle a b c

-- ----------------------------------------------------------------- [ Parsers ]

||| Parse three side lengths.
partial sides : Parser (Vect 3 Integer)
sides = ntimes 3 (spaces *> integer)

{-
||| Parse three side lengths and construct a `Triangle`.
partial triangle : Parser Triangle
triangle = do [a,b,c] <- sides
              pure $ triangle a b c
-}

-- ------------------------------------------------------------------- [ Logic ]

||| Given a foldable structure containing `Triangle`s,
||| count which of them are not `Illogical`.
countPossible : Foldable t => t Triangle -> Nat
countPossible = foldl go 0
  where
    go : Nat -> Triangle -> Nat
    go n (Illogical _ _ _) = n
    go n _                 = S n

-- ---------------------------------------------------------------- [ Part One ]

partial partOne : List (Vect 3 Integer) -> IO String
partOne = pure . show . countPossible . map fromVect

-- ---------------------------------------------------------------- [ Part Two ]

partial partTwo : List (Vect 3 Integer) -> IO String
partTwo = pure . show . countPossible .
          concatMap (toList . map fromVect . transpose) .
          go
  where
    go : List (Vect 3 Integer) -> List (Vect 3 (Vect 3 Integer))
    go (x::y::z::zs) = [x,y,z] :: go zs
    go _             = []

-- -------------------------------------------------------------------- [ Main ]

namespace Main

  partial main : IO ()
  main = runDay $ MkDay 3
         (some (sides <* spaces))
         partOne
         partTwo

-- --------------------------------------------------------------------- [ EOF ]
