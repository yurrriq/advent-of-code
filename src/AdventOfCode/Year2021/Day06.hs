{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AdventOfCode.Year2021.Day06 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Lens ((%~))
import Data.Finite (Finite)
import Data.Maybe (fromJust, mapMaybe)
import Data.Semigroup (stimes)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Sized qualified as VGS
import Data.Vector.Sized qualified as VS
import GHC.TypeNats (KnownNat)
import Linear (Additive (..), negated, scaled, (!*), (!*!))
import Text.Trifecta (commaSep, natural)

type Lanternfish = Finite 9

type State = VS.Vector 9 Int

newtype STM n = STM {unSTM :: VS.Vector n (VS.Vector n Int)}
  deriving (Eq, Show)

instance (Functor v, KnownNat n, forall a. VG.Vector v a) => Additive (VGS.Vector v n) where
  zero = VGS.replicate 0
  liftU2 = VGS.zipWith
  liftI2 = VGS.zipWith

instance (KnownNat n) => Num (STM n) where
  STM x + STM y = STM (x ^+^ y)
  STM x - STM y = STM (x ^-^ y)
  STM x * STM y = STM (x !*! y)
  negate = STM . negated . unSTM
  abs = undefined
  signum = undefined
  fromInteger = STM . scaled . fromInteger

instance (KnownNat n) => Semigroup (STM n) where
  STM x <> STM y = STM (x !*! y)

main :: IO ()
main = $(defaultMain)

getInput :: IO [Lanternfish]
getInput = parseInput (commaSep (fromInteger <$> natural)) $(inputFilePath)

example :: [Lanternfish]
example = [3, 4, 3, 1, 2]

partOne :: [Lanternfish] -> Int
partOne = simulate 80

partTwo :: [Lanternfish] -> Int
partTwo = simulate 256

simulate :: Int -> [Lanternfish] -> Int
simulate n = sum . (unSTM (stm ^ n) !*) . mkState

ssimulate :: Int -> [Lanternfish] -> Int
ssimulate n = sum . (unSTM (stimes n stm) !*) . mkState

mkState :: [Lanternfish] -> State
mkState = foldr ((%~ succ) . VS.ix) (pure 0)

stm :: STM 9
stm =
  STM
    . fromJust
    . VS.fromListN
    . mapMaybe VS.fromListN
    $ [ [0, 1, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 1, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 1, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 1, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 1, 0, 0],
        [1, 0, 0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 1],
        [1, 0, 0, 0, 0, 0, 0, 0, 0]
      ]
