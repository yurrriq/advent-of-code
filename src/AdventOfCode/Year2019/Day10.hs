{-# LANGUAGE BlockArguments #-}

module AdventOfCode.Year2019.Day10 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Lens (ifoldl', makeLenses, over, views, (%~), _1, _2, _3, _4)
import Data.Bifunctor (bimap, first)
import Data.Function (on)
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as HM
import Data.List (sort)
import Data.List.Extra (maximumOn, minimumOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio (denominator, numerator)
import Data.Set qualified as Set
import GHC.Real (Ratio (..))
import Linear (V2 (..), V4 (..), distance)
import Text.Trifecta (Parser, char, newline, sepEndBy, some)

type DetectionMap = HashMap Angle (Map Location Distance)

type Location = V2 Int

type Angle = Ratio Int

type Distance = Double

data Grid
  = Grid
  { _dimensions :: !(V2 Int),
    _stations :: !(Map Location DetectionMap)
  }
  deriving (Eq)

makeLenses ''Grid

instance Show Grid where
  show (Grid (V2 width height) asteroids) =
    concat $
      [ '\n'
          : concat
            [ maybe "." (show . HM.size) (Map.lookup (V2 x y) asteroids)
            | x <- [0 .. width - 1]
            ]
      | y <- [0 .. height - 1]
      ]

main :: IO ()
main = $(defaultMain)

getInput :: IO Grid
getInput = parseInput grid $(inputFilePath)

partOne :: Grid -> Int
partOne = HM.size . snd . bestLocation

partTwo :: Grid -> Int
partTwo asteroidBelt = x * 100 + y
  where
    (_from, visible) = bestLocation asteroidBelt
    angles = clockwise (HM.keys visible)
    V2 x y = vaporize visible angles !! 199

grid :: Parser Grid
grid = gridDetection . mkGrid <$> some asteroid `sepEndBy` newline

mkGrid :: [[Bool]] -> Grid
mkGrid = uncurry Grid . ifoldl' (ifoldl' . go) (0, Map.empty)
  where
    go y x = flip \case
      True -> bimap (const position) (Map.insert position HM.empty)
      False -> first (const position)
      where
        position = V2 x y

asteroid :: Parser Bool
asteroid = (char '.' $> False) <|> (char '#' $> True)

bestLocation :: Grid -> (Location, DetectionMap)
bestLocation = views stations (maximumOn (HM.size . snd) . Map.toList)

vaporize :: DetectionMap -> [Angle] -> [Location]
vaporize visible allAngles =
  reverse . fst $
    until
      (all Map.null . snd)
      (flip (foldl' go) allAngles)
      ([], visible)
  where
    go :: ([Location], DetectionMap) -> Angle -> ([Location], DetectionMap)
    go (vaporized, remaining) angle
      | Map.null targets = (vaporized, remaining)
      | otherwise = (target : vaporized, HM.adjust (Map.delete target) angle remaining)
      where
        targets = remaining ! angle
        (target, _) = minimumOn snd (Map.toList targets)

clockwise :: [Angle] -> [Angle]
clockwise = foldMap sort . foldr go (pure @V4 [])
  where
    go angle =
      flip over (angle :) $
        case (isRightward angle, isUpward angle) of
          (True, True) -> _1
          (True, False) -> _2
          (False, False) -> _3
          (False, True) -> _4

isRightward :: Angle -> Bool
isRightward = (>= 0) . denominator

isUpward :: Angle -> Bool
isUpward = (<= 0) . numerator

gridDetection :: Grid -> Grid
gridDetection =
  stations %~ \asteroids ->
    flip Map.mapWithKey asteroids \from detections ->
      foldl' (detect from) detections $
        Set.delete from (Map.keysSet asteroids)

detect :: Location -> DetectionMap -> Location -> DetectionMap
detect from detections to = HM.alter go angle detections
  where
    dist = (distance @V2 @Double `on` fmap fromIntegral) from to
    angle = case to - from of
      V2 0 y -> signum y :% 0
      V2 x 0 -> 0 :% signum x
      V2 x y -> let d = gcd x y in (y `quot` d) :% (x `quot` d)
    go = pure . maybe (Map.singleton to dist) (Map.insert to dist)
