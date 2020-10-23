{-# OPTIONS_GHC -Wno-type-defaults #-}

module AdventOfCode.Year2019.Day10 where

import AdventOfCode.Util (parseInput)
import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Foldable (minimumBy)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict ((!), HashMap)
import qualified Data.HashSet as HS
import Data.List (delete, maximumBy, sortBy)
import Data.Ord (comparing)
import GHC.Real (Ratio (..))
import Text.Trifecta (Parser, char, newline, sepEndBy, some)

partOne :: IO Int
partOne = HM.size . snd . bestLocation <$> parseInput grid "input/2019/day10.txt"

partTwo :: IO Int
partTwo =
  do
    asteroidBelt <- parseInput grid "input/2019/day10.txt"
    let (_from, visible) = bestLocation asteroidBelt
    let angles = clockwise (HM.keys visible)
    let (x, y) = vaporize visible angles !! 199
    pure (x * 100 + y)

data Grid
  = Grid (Int, Int) (HashMap Location (HashMap Angle (HashMap Location Distance)))
  deriving (Eq)

instance Show Grid where
  show (Grid (width, height) asteroids) =
    concat $
      [ '\n'
          : concat
            [ maybe "." (show . HM.size) (HM.lookup (x, y) asteroids)
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
    let inner y z x = pure (HM.insert (x, y) ((input !! y) !! x, HS.empty) z)
    let outer z y = foldM (inner y) z [0 .. width - 1]
    gridDetection
      . Grid (width, height)
      . HM.map (const HM.empty)
      . HM.filter (id . fst)
      <$> foldM outer HM.empty [0 .. height - 1]

asteroid :: Parser Bool
asteroid = (char '.' *> pure False) <|> (char '#' *> pure True)

bestLocation :: Grid -> (Location, HashMap Angle (HashMap Location Distance))
bestLocation (Grid _ asteroids) = maximumBy go (HM.toList asteroids)
  where
    go (_, xmap) (_, ymap) = comparing HM.size xmap ymap

vaporize :: HashMap Angle (HashMap Location Distance) -> [Angle] -> [Location]
vaporize visible allAngles = go [] visible allAngles
  where
    go :: [Location] -> HashMap Angle (HashMap Location Distance) -> [Angle] -> [Location]
    go vaporized remaining [] =
      if all HM.null remaining
        then reverse vaporized
        else go vaporized remaining allAngles
    go vaporized remaining (a : as) =
      let targets = (remaining ! a)
       in if HM.null targets
            then go vaporized remaining as
            else
              let (target, _) = minimumBy (comparing snd) (HM.toList targets)
               in go (target : vaporized) (HM.adjust (HM.delete target) a remaining) as

clockwise :: [Angle] -> [Angle]
clockwise angles =
  let (i, ii, iii, iv) = foldl go ([], [], [], []) angles
   in sortBy (comparing (fromRational)) i
        ++ sortBy (comparing fromRational) iv
        ++ sortBy (comparing fromRational) iii
        ++ sortBy (comparing fromRational) ii
  where
    go (i, ii, iii, iv) a@(n :% d)
      | n <= 0 && d >= 0 = (a : i, ii, iii, iv)
      | n > 0 && d >= 0 = (i, ii, iii, a : iv)
      | n >= 0 && d <= 0 = (i, ii, a : iii, iv)
      | n < 0 && d < 0 = (i, a : ii, iii, iv)
      | otherwise = error (show a)

gridDetection :: Grid -> Grid
gridDetection (Grid dimensions asteroids) =
  Grid dimensions (HM.mapWithKey stationDetection asteroids)
  where
    stationDetection from knownDetections =
      foldl (detect from) knownDetections (delete from (HM.keys asteroids))

detect :: Location -> HashMap Angle (HashMap Location Distance) -> Location -> HashMap Angle (HashMap Location Distance)
detect (fromX, fromY) detections (toX, toY) = HM.alter go angle detections
  where
    distance = sqrt (fromIntegral dX ** 2 + fromIntegral dY ** 2)
    angle =
      if dX == 0
        then
          if dY > 0
            then 1 :% 0
            else (-1) :% 0
        else
          if dY == 0
            then
              if dX > 0
                then 0 :% 1
                else 0 :% (-1)
            else reduce dY dX
    dX = fromIntegral (toX - fromX)
    dY = fromIntegral (toY - fromY)
    reduce x y = (x `quot` d) :% (y `quot` d)
      where
        d = gcd x y
    go Nothing = Just (HM.singleton (toX, toY) distance)
    go (Just visible) = Just (HM.insert (toX, toY) distance visible)
