{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2021.Day11 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (neighborsOf)
import Control.Lens (ifoldl', makeLenses, use, uses, (%=), (+=), (.=), (<~))
import Control.Monad.Extra (whileM)
import Data.Char (digitToInt)
import Data.Finite (Finite, finites, getFinite, modulo)
import Data.Ix (Ix, inRange)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Generic.Data (GenericProduct (..))
import Linear (V2 (..))
import Relude
import Text.Trifecta (Parser, digit, newline, sepEndBy)

type EnergyLevel = Finite 10

type Grid = Map (V2 Int) EnergyLevel

grid :: Parser Grid
grid = mkGrid <$> some (fromIntegral . digitToInt <$> digit) `sepEndBy` newline

mkGrid :: [[EnergyLevel]] -> Grid
mkGrid = ifoldl' (ifoldl' . f) Map.empty
  where
    f y x = flip (Map.insert (V2 x y))

printGrid :: Grid -> IO ()
printGrid m = forM_ (finites @10) $ \y -> do
  forM_ (finites @10) $ \x ->
    putStr (show (getFinite (m Map.! (fromIntegral <$> V2 x y))))
  putStrLn ""

data PuzzleState' a
  = PuzzleState
  { _theCount :: !a,
    _theGrid :: !Grid
  }
  deriving (Eq, Generic, Show)
  deriving
    (Semigroup, Monoid)
    via (GenericProduct (PuzzleState' (Sum a)))

makeLenses ''PuzzleState'

type PuzzleState = PuzzleState' Int

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO Grid
getInput = parseInputAoC 2021 11 grid

getExample :: IO Grid
getExample = parseString grid example

example :: String
example =
  "5483143223\n\
  \2745854711\n\
  \5264556173\n\
  \6141336146\n\
  \6357385478\n\
  \4167524645\n\
  \2176841721\n\
  \6882881134\n\
  \4846848554\n\
  \5283751526\n"

partOne :: Puzzle Grid PuzzleState Int
partOne = do
  theGrid <~ ask
  runSteps 100

partTwo :: Puzzle Grid PuzzleState Int
partTwo = do
  theGrid <~ ask
  theCount .= 0
  whileM
    $ stepM (const 1)
    *> uses theGrid (any (/= 0))
  use theCount

runSteps :: Int -> Puzzle Grid PuzzleState Int
runSteps 0 = use theCount
runSteps k = stepM Set.size *> runSteps (k - 1)

stepM :: (MonadState PuzzleState m) => (Set (V2 Int) -> Int) -> m ()
stepM f = do
  theGrid %= fmap increase
  (flashes, steppedGrid) <- uses theGrid (handleFlashes Set.empty)
  theGrid .= steppedGrid
  theCount += f flashes

increase :: EnergyLevel -> EnergyLevel
increase = modulo . succ . getFinite

handleFlashes :: Set (V2 Int) -> Grid -> (Set (V2 Int), Grid)
handleFlashes seen current =
  Map.foldlWithKey flash (seen, current) current & \(flashes, next) ->
    (flashes, foldl' (\m coords -> Map.insert coords 0 m) next flashes)

flash :: (Set (V2 Int), Grid) -> V2 Int -> EnergyLevel -> (Set (V2 Int), Grid)
flash (flashes, octopuses) coords octopus
  | octopus == 0 && coords `Set.notMember` flashes =
      handleFlashes (Set.insert coords flashes)
        $ foldl' (flip (Map.alter (fmap increase))) octopuses
        $ neighborsInRange (0, 9) coords
  | otherwise = (flashes, octopuses)

neighborsInRange :: (Ix a, Num a) => (V2 a, V2 a) -> V2 a -> Set (V2 a)
neighborsInRange range point = Set.filter (inRange range) (neighborsOf point)
