-- --------------------------------------------------------------- [ Day08.idr ]
-- Module      : Data.Advent.Day08
-- Description : My solution to the Day 8 puzzle of the 2016 Advent of Code.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : http://adventofcode.com/2016/day/7
-- --------------------------------------------------------------------- [ EOH ]
||| Day 8: Two-Factor Authentication
module Data.Advent.Day08

import Data.Vect

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings
import public Lightyear.StringFile

%default total

-- -------------------------------------------------------------- [ Data Types ]

%access public export

data Pixel = Off | On

implementation Eq Pixel where
    On  == On  = True
    Off == Off = True
    px1 == px2 = False

implementation Show Pixel where
    show Off = "."
    show On  = "#"

Screen : Nat -> Nat -> Type
Screen w h = Vect h (Vect w Pixel)

implementation [showScreen] Show (Screen w h) where
    show = unlines . toList . map (concatMap show)

data Rect : Nat -> Nat -> Type where
     MkRect : Fin w -> Fin h -> Rect w h

implementation Show (Fin n) where
    show = show . finToNat

-- showFin : Fin n -> String
-- showFin = show . finToNat

implementation Show (Rect w h) where
    show (MkRect w h) = "rect " ++ show w ++ "x" ++ show h

data Rotate : Nat -> Nat -> Type where
     Row : Fin w -> Fin h -> Rotate h w
     Col : Fin w -> Fin h -> Rotate w h

implementation Show (Rotate xy by) where
    show (Row y by) = "rotate row y=" ++ show y ++ " by " ++ show by
    show (Col x by) = "rotate column x=" ++ show x ++ " by " ++ show by

-- ----------------------------------------------------------------- [ Parsers ]

%access export

partial
side : (n : Nat) -> Parser (Fin n)
side n = do Just len <- (\m => integerToFin m n) <$> integer
              | Nothing => fail "out of bounds"
            pure len

partial
rect : Parser (Rect w h)
rect = liftA2 MkRect (token "rect" *> side w)
                     (token "x"    *> side h)  <?> "rect AxB"

partial
maybeFin : (n : Nat) -> Parser (Maybe (Fin n))
maybeFin n = marshal <$> integer <* spaces <?>
             "a number strictly less than " ++ show n
  where
    marshal : Integer -> Maybe (Fin n)
    marshal m = integerToFin m n

partial
row : Parser (Rotate w h)
row {w} {h} = do Just y  <- token "rotate row y=" *> maybeFin h
                 Just by <- token "by" *> maybeFin w
                 pure $ Row y by

partial
column : Parser (Rotate w h)
column {w} {h} = do Just x  <- token "rotate column x=" *> maybeFin w
                    Just by <- token "by" *> maybeFin h
                    pure $ Col x by

partial
rotation : Parser (Rotate w h)
rotation = row <|> column <?> "a rotation"

partial
rectOrRotation : Parser (Either (Rect w h) (Rotate w h))
rectOrRotation {w} {h} = case !(opt (rect {w} {h})) of
                              Just rec => pure $ Left rec
                              Nothing  => Right <$> rotation {w} {h}

-- ----------------------------------------------------------------- [ Helpers ]

%access private

mapIndexed : (f : (a, Nat) -> a) -> Vect n a -> Vect n a
mapIndexed f xs {n} = map f (zip xs (go n Z))
  where
    go : (m : Nat) -> Nat -> Vect m Nat
    go Z _     = []
    go (S k) x = x :: go k (S x)

-- ------------------------------------------------------------------ [ Proofs ]

%access private

finToLTE : (f : Fin n) -> LTE (finToNat f) n
finToLTE FZ               = LTEZero
finToLTE (FS _) {n = S _} = LTESucc (finToLTE _)

total compositeLemma : (left, right : Nat) -> {auto smaller : LTE left right} ->
  (k : Nat ** right = left + k)
compositeLemma left right = (right - left ** go left right)
  where
    go : (left, right : Nat) -> {auto smaller : LTE left right} ->
      right = left + (right - left)
    go Z right        = rewrite minusZeroRight right in Refl
    go (S k) (S j) {smaller} =
        let inductiveHypothesis = go k j {smaller = fromLteSucc smaller} in
            eqSucc j (k + (minus j k)) inductiveHypothesis

total updateLemma : (prf : n = finToNat by + m) -> n = m + finToNat by
updateLemma {by} {m} prf = rewrite plusCommutative m (finToNat by) in prf

-- -- ---------------------------------------------------------------- [ Logic ]

turnOn : Rect sw sh -> Screen sw sh -> Screen sw sh
turnOn (MkRect w h) = mapIndexed f
  where
    g : (Pixel, Nat) -> Pixel
    g (col, x) = if x < (finToNat w) then On else col
    f : (Vect m Pixel, Nat) -> Vect m Pixel
    f (row, y) = if y < (finToNat h) then mapIndexed g row else row

namespace Vect

  rotate : (by : Fin n) -> Vect (finToNat by + m) a ->
           (m ** Vect (m + finToNat by) a)
  rotate by xs {m} = let (ys,zs) = splitAt (finToNat by) xs in
                         (m ** zs ++ ys)

updateRow : (by : Fin n) -> (row : Vect n Pixel) -> Vect n Pixel
updateRow {n} by row =
    let (m ** prf)    = compositeLemma (finToNat by) n {smaller = finToLTE by}
        (_ ** newRow) = rotate by $ the (Vect (finToNat by + m) _) $
                        rewrite sym prf in row in
                        rewrite updateLemma prf in
                                newRow

namespace Fin

    rotate : Fin n -> Fin n
    rotate f {n} = let x = (-) n (finToNat f) {smaller = finToLTE f} in
                       fromMaybe f (natToFin x n) -- HACK

namespace Screen

  empty : {w, h : Nat} -> Screen w h
  empty {w} {h} = replicate h (replicate w Off)

  rotate : (Rotate w h) -> Screen w h -> Screen w h
  rotate (Row y by) screen     = updateAt y (updateRow (rotate by)) screen
  rotate (Col x by) screen {h} = transpose $
                                 updateAt x (updateRow (rotate by)) $
                                 transpose screen

howManyLit : Screen w h -> Nat
howManyLit = foldr ((+) . foldr go Z) Z
  where
    go : Pixel -> Nat -> Nat
    go On  = S
    go Off = id

justDoIt : List (Either (Rect w h) (Rotate w h)) -> Screen w h
justDoIt = foldl (\s => either (flip turnOn s) (flip rotate s)) empty

-- ---------------------------------------------------------------- [ Examples ]

ex1 : Screen 7 3
ex1 = turnOn (MkRect 3 2) empty

ex2 : Screen 7 3
ex2 = rotate (Col 1 1) ex1

ex3 : Screen 7 3
ex3 = rotate (Row 0 4) ex2

ex4 : Screen 7 3
ex4 = rotate (Col 1 1) ex3

partial
examples : Either String (List (Either (Rect 7 3) (Rotate 7 3)))
examples = parse (some (rectOrRotation <* spaces)) $ unlines [
             "rect 3x2",
             "rotate column x=1 by 1",
             "rotate row y=0 by 4",
             "rotate column x=1 by 1"
           ]

||| Convenient representation of Part One examples for the REPL.
partial
prettyExamples : Either String (List (List String))
prettyExamples = transpose .
                 map (lines . show @{showScreen}) .
                 scanl (\s => either (flip turnOn s) (flip rotate s)) empty <$>
                 examples

-- -------------------------------------------------------------- [ Main Logic ]

%access export

main' : Show b => Parser a -> (a -> b) -> IO ()
main' p f =
    either putStrLn (printLn . f)
           !(run $ parseFile (const show) (const id) p "input/day08.txt")

-- ---------------------------------------------------------------- [ Part One ]

namespace PartOne

    partial main : IO ()
    main = main' (some (rectOrRotation {w=50} {h=6} <* spaces))
                 (howManyLit . justDoIt)

-- -------------------------------------------------------------------- [ Main ]

namespace Main

    partial main : IO ()
    main = putStr "Part One: " *> PartOne.main

-- --------------------------------------------------------------------- [ EOF ]
