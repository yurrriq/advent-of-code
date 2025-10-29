module AdventOfCode.Year2015.Day20 where

import AdventOfCode.TH (defaultMain)
import Data.Euclidean (GcdDomain)
import Data.List.Infinite (Infinite (..), (...))
import Data.List.Infinite qualified as Infinite
import Math.NumberTheory.ArithmeticFunctions (sigma)
import Math.NumberTheory.Primes (UniqueFactorisation)

main :: IO ()
main = $(defaultMain)

getInput :: IO Int
getInput = pure 33100000

-- | Find the lowest house number of the house to get at least a given number of
-- presents when the Elves are sent down a street with infinite houses.
--
-- See 'deliverInfinite'.
partOne :: Int -> Int
partOne = findHouse deliverInfinite

-- | The number of presents the house with a given number gets when the Elves
-- are sent down a street with infinite houses.
--
-- See 'tenSigma'.
deliverInfinite :: Int -> Int
deliverInfinite = tenSigma

-- | See 'a326122'.
tenSigma :: (UniqueFactorisation n, Integral n, Num a, GcdDomain a) => n -> a
tenSigma = (10 *) . sigma 1

-- | \(a(n) = 10\sigma(n)\), i.e. [A326122](https://oeis.org/A326122).
a326122 :: forall a. (UniqueFactorisation a, Integral a, GcdDomain a) => Infinite a
a326122 = Infinite.map (tenSigma @a) (1 ...)

-- | Find the lowest house number of the house to get at least a given number of
-- presents when each Elf stops after delivering presents to \(50\) houses.
--
-- See 'deliverFinite'.
partTwo :: Int -> Int
partTwo = findHouse deliverFinite

-- | The number of presents the house with a given number gets when each Elf
-- stops after delivering presents to \(50\) houses.
--
-- See 'elevenSigma'.
deliverFinite :: Int -> Int
deliverFinite = elevenSigma

-- | \(11\sigma(n) = 10\sigma(n) + \sigma(n)\), i.e., @Infinite.zipWith (+)
-- a326122 a000203@.
elevenSigma :: (UniqueFactorisation n, Integral n, Num a, GcdDomain a) => n -> a
elevenSigma = (11 *) . sigma 1

-- | The sum of the divisors of \(n\), i.e., [A000203](https://oeis.org/A000203).
a000203 :: forall a. (UniqueFactorisation a, Integral a, GcdDomain a) => Infinite a
a000203 = Infinite.map (sigma @a 1) (1 ...)

-- | Given a function that computes the number of presents the house with a
-- given number gets, find the lowest house number of the house to get at least
-- a given number of presents.
--
-- See 'deliverInfinite' and 'deliverFinite'.
findHouse :: (Int -> Int) -> Int -> Int
findHouse housePresents minimumPresents =
  Infinite.find ((>= minimumPresents) . housePresents) (1 ...)
