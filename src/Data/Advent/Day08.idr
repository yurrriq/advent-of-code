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

Screen : Type
Screen = Vect 3 (Vect 7 Pixel)

implementation [showScreen] Show Screen where
    show = unlines . toList . map (concatMap show)

data Rect = MkRect (Fin 7) (Fin 3)

implementation Show Rect where
    show (MkRect w h) = "rect " ++ showFin w ++ "x" ++ showFin h
      where
        showFin : Fin n -> String
        showFin = show . finToNat

data Rotate = Row (Fin 3) (Fin 7)
            | Col (Fin 7) (Fin 3)

-- ----------------------------------------------------------------- [ Parsers ]

%access export

partial
side : (n : Nat) -> Parser (Fin n)
side n = do Just len <- (\m => integerToFin m n) <$> integer
              | Nothing => fail "out of bounds"
            pure len

partial
rect : Parser Rect
rect = liftA2 MkRect (token "rect" *> side 7)
                     (token "x"    *> side 3)  <?> "rect AxB"

partial
maybeFin : (n : Nat) -> Parser (Maybe (Fin n))
maybeFin n = marshal <$> integer <* spaces <?>
             "a number strictly less than " ++ show n
  where
    marshal : Integer -> Maybe (Fin n)
    marshal m = integerToFin m n

partial
row : Parser Rotate
row = do Just y  <- token "rotate row y=" *> maybeFin 3
         Just by <- token "by" *> maybeFin 7
         pure $ Row y by

partial
column : Parser Rotate
column = do Just x  <- token "rotate column x=" *> maybeFin 7
            Just by <- token "by" *> maybeFin 3
            pure $ Col x by

partial
rotation : Parser Rotate
rotation = row <|> column <?> "a rotation"

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

total lemma : (left, right : Nat) -> {auto smaller : LTE left right} ->
  (k : Nat ** right = left + k)
lemma left right = (right - left ** go left right)
  where
    go : (left, right : Nat) -> {auto smaller : LTE left right} ->
      right = left + (right - left)
    go Z right        = rewrite minusZeroRight right in Refl
    go (S k) (S j) {smaller} =
        let inductiveHypothesis = go k j {smaller = fromLteSucc smaller} in
            eqSucc j (k + (minus j k)) inductiveHypothesis

-- -- ------------------------------------------------------------------- [ Logic ]

turnOn : Rect -> Screen -> Screen
turnOn (MkRect w h) = mapIndexed f
  where
    g : (Pixel, Nat) -> Pixel
    g (col, x) = if x < (finToNat w) then On else col
    f : (Vect m Pixel, Nat) -> Vect m Pixel
    f (row, y) = if y < (finToNat h) then mapIndexed g row else row

namespace Vect

  -- rotate : (by : Fin n ) -> Vect (finToNat by + m) a -> Vect (finToNat by + m) a
  -- rotate by = uncurry (++) . splitAt (finToNat by) . reverse
  rotate : (by : Fin n ) -> Vect (finToNat by + m) a -> (m ** Vect (finToNat by + m) a)
  rotate by xs {m} = let (ys,zs) = splitAt (finToNat by) (reverse xs) in (m ** ys ++ zs)

updateRow : (by : Fin n) -> (row : Vect n Pixel) -> Vect n Pixel
updateRow {n} by row =
    let (m ** prf)    = lemma (finToNat by) n {smaller = finToLTE by}
        (_ ** newRow) = rotate by $ the (Vect (finToNat by + m) _) $
                        rewrite sym prf in row in
        rewrite prf in newRow

namespace Screen

  empty : Screen
  empty = replicate 3 (replicate 7 Off)

  rotate : Rotate -> Screen -> Screen
  rotate (Row y by) screen = updateAt y (updateRow by) screen
  rotate (Col x by) screen = let xs = transpose screen
                                 ys = updateAt x (updateRow by) xs in
                                 transpose ys

-- --------------------------------------------------------------------- [ EOF ]
