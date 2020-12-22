{-# OPTIONS_GHC -Wno-type-defaults #-}

module AdventOfCode.Year2019.Day10 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Foldable (minimumBy)
import Data.Functor (($>))
import Data.List (delete, maximumBy, sortOn)
import qualified Data.Map as Map
import Data.Map ((!), Map)
import Data.Ord (comparing)
import qualified Data.Set as Set
import GHC.Real (Ratio (..))
import Text.Trifecta (Parser, char, newline, sepEndBy, some)

main :: IO ()
main =
  do
    putStrLn "[2019] Day 10: Monitoring Station"
    input <- getInput
    putStr "Part One: "
    print (partOne input)
    putStr "Part Two: "
    print (partTwo input)

getInput :: IO Grid
getInput = parseInput grid $(inputFilePath)

partOne :: Grid -> Int
partOne = Map.size . snd . bestLocation

partTwo :: Grid -> Int
partTwo asteroidBelt = x * 100 + y
  where
    (_from, visible) = bestLocation asteroidBelt
    angles = clockwise (Map.keys visible)
    (x, y) = vaporize visible angles !! 199

data Grid
  = Grid (Int, Int) (Map Location (Map Angle (Map Location Distance)))
  deriving (Eq)

instance Show Grid where
  show (Grid (width, height) asteroids) =
    concat $
      [ '\n'
          : concat
            [ maybe "." (show . Map.size) (Map.lookup (x, y) asteroids)
              | x <- [0 .. width - 1]
            ]
        | y <- [0 .. height -1]
      ]

type Location = (Int, Int)

type Angle = Ratio Integer

type Distance = Double

grid :: Parser Grid
grid =
  do
    input <- some asteroid `sepEndBy` newline
    let height = length input
    let width = length (head input)
    let inner y z x = pure (Map.insert (x, y) ((input !! y) !! x, Set.empty) z)
    let outer z y = foldM (inner y) z [0 .. width - 1]
    gridDetection
      . Grid (width, height)
      . fmap (const Map.empty)
      . Map.filter fst
      <$> foldM outer Map.empty [0 .. height - 1]

asteroid :: Parser Bool
asteroid = (char '.' $> False) <|> (char '#' $> True)

bestLocation :: Grid -> (Location, Map Angle (Map Location Distance))
bestLocation (Grid _ asteroids) = maximumBy go (Map.toList asteroids)
  where
    go (_, xmap) (_, ymap) = comparing Map.size xmap ymap

vaporize :: Map Angle (Map Location Distance) -> [Angle] -> [Location]
vaporize visible allAngles = go [] visible allAngles
  where
    go :: [Location] -> Map Angle (Map Location Distance) -> [Angle] -> [Location]
    go vaporized remaining [] =
      if all Map.null remaining
        then reverse vaporized
        else go vaporized remaining allAngles
    go vaporized remaining (a : as) =
      let targets = (remaining ! a)
       in if Map.null targets
            then go vaporized remaining as
            else
              let (target, _) = minimumBy (comparing snd) (Map.toList targets)
               in go (target : vaporized) (Map.adjust (Map.delete target) a remaining) as

clockwise :: [Angle] -> [Angle]
clockwise angles =
  let (i, ii, iii, iv) = foldl go ([], [], [], []) angles
   in sortOn fromRational i
        ++ sortOn fromRational iv
        ++ sortOn fromRational iii
        ++ sortOn fromRational ii
  where
    go (i, ii, iii, iv) a@(n :% d)
      | n <= 0 && d >= 0 = (a : i, ii, iii, iv)
      | n > 0 && d >= 0 = (i, ii, iii, a : iv)
      | n >= 0 && d <= 0 = (i, ii, a : iii, iv)
      | n < 0 && d < 0 = (i, a : ii, iii, iv)
      | otherwise = error (show a)

gridDetection :: Grid -> Grid
gridDetection (Grid dimensions asteroids) =
  Grid dimensions (Map.mapWithKey stationDetection asteroids)
  where
    stationDetection from knownDetections =
      foldl (detect from) knownDetections (delete from (Map.keys asteroids))

detect :: Location -> Map Angle (Map Location Distance) -> Location -> Map Angle (Map Location Distance)
detect (fromX, fromY) detections (toX, toY) = Map.alter go angle detections
  where
    distance = sqrt (fromIntegral dX ** 2 + fromIntegral dY ** 2)
    angle
      | dX == 0 = if dY > 0 then 1 :% 0 else (-1) :% 0
      | dY == 0 = if dX > 0 then 0 :% 1 else 0 :% (-1)
      | otherwise = reduce dY dX
    dX = fromIntegral (toX - fromX)
    dY = fromIntegral (toY - fromY)
    reduce x y = (x `quot` d) :% (y `quot` d)
      where
        d = gcd x y
    go Nothing = Just (Map.singleton (toX, toY) distance)
    go (Just visible) = Just (Map.insert (toX, toY) distance visible)
