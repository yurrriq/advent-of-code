{-# LANGUAGE DataKinds #-}

module AdventOfCode.Year2018.Day05
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.TH (inputFilePath)
import Data.Algebra.Free (foldMapFree, returnFree)
import Data.Char (isLower, isUpper, ord, toLower)
import Data.Finite (Finite, finites, packFinite)
import Data.Function (on)
import Data.Group (invert)
import Data.Group.Free (FreeGroupL)
import qualified Data.Group.Free as FG

-- | Units' types are represented by letters, modelled by a finite number type
-- inhabited by exactly 26 values.
type Unit = Finite 26

fromChar :: Char -> Maybe (Either Unit Unit)
fromChar c
  | isLower c = Left <$> unit
  | isUpper c = Right <$> unit
  | otherwise = Nothing
  where
    unit = packFinite . fromIntegral $ ((-) `on` ord) (toLower c) 'a'

inject :: Char -> FreeGroupL Unit
inject = foldMap (either returnFree (invert . returnFree)) . fromChar

clean :: Unit -> FreeGroupL Unit -> FreeGroupL Unit
clean c = foldMapFree go
  where
    go :: Unit -> FreeGroupL Unit
    go d
      | d == c = mempty
      | otherwise = returnFree d

-- | Compute the order of a 'FreeGroupL'.
order :: FreeGroupL a -> Int
order = length . FG.toList

partOne :: String -> Int
partOne = order . foldMap inject

partTwo :: String -> Int
partTwo = minimum . cleanedPolymers . foldMap inject
  where
    cleanedPolymers :: FreeGroupL Unit -> [Int]
    cleanedPolymers polymer = order . flip clean polymer <$> finites

main :: IO ()
main =
  do
    input <- readFile $(inputFilePath)
    putStr "Part One: "
    print (partOne input)
    putStr "Part Two: "
    print (partTwo input)
