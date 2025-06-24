{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module AdventOfCode.Util
  ( CyclicEnum (..),
    Frequencies,
    frequencies,
    frequenciesInt,
    maybeParseByteString,
    findFirstDup,
    hammingDistance,
    hammingSimilar,
    iterateMaybe,
    scan,
    count,
    snoc,
    wigglesum,
    fix',
    adjacencies,
    neighborsOf,
    holes,
    numDigits,
    (<&&>),
    (<||>),
    (<.>),
  )
where

import Control.Arrow (second, (>>>))
import Control.Comonad.Store (experiment)
import Control.Lens (holesOf)
import Control.Monad (join, (>=>))
import Data.ByteString (ByteString)
import Data.Function (fix)
import Data.IntMap qualified as IM
import Data.List (unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Trifecta (Parser, Result (..), parseByteString)

-- https://github.com/bravit/hid-examples/blob/master/ch02/radar/Radar.hs
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred d
    | d == minBound = maxBound
    | otherwise = pred d

  csucc :: a -> a
  csucc d
    | d == maxBound = minBound
    | otherwise = succ d

type Frequencies a = Map a Int

frequencies :: (Ord a) => [a] -> Frequencies a
frequencies = foldr go Map.empty
  where
    go k = Map.insertWith (+) k 1

frequenciesInt :: (Foldable t) => t Int -> IM.IntMap Int
frequenciesInt = foldr go IM.empty
  where
    go k = IM.insertWith (+) k 1

maybeParseByteString :: Parser a -> ByteString -> Maybe a
maybeParseByteString p =
  parseByteString p mempty >>> \case
    Failure _ -> Nothing
    Success res -> Just res

hammingDistance :: (Eq a) => [a] -> [a] -> Maybe Integer
hammingDistance (x : xs) (y : ys)
  | x /= y = (+ 1) <$> recur
  | otherwise = recur
  where
    recur = hammingDistance xs ys
hammingDistance [] [] = Just 0
hammingDistance _ _ = Nothing

hammingSimilar :: (Eq a) => Integer -> [a] -> [a] -> Bool
hammingSimilar n xs = maybe False (<= n) . hammingDistance xs

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : unfoldr (fmap (join (,)) . f) x

findFirstDup :: (Ord a) => [a] -> Maybe a
findFirstDup = go Set.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | x `Set.member` seen = Just x
      | otherwise = go (Set.insert x seen) xs

scan :: (Monoid m) => [m] -> [m]
scan = scanl mappend mempty

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

wigglesum :: (Traversable t) => (a -> [a]) -> t a -> [t a]
wigglesum wiggle = holesOf traverse >=> experiment wiggle

fix' :: (Eq a) => (a -> a) -> a -> a
fix' f = fix (\g !x -> let fx = f x in if fx == x then x else g fx)

adjacencies :: (Applicative f, Num a, Eq (f a), Traversable f) => [f a]
adjacencies = filter (/= pure 0) $ sequenceA (pure [-1, 0, 1])

neighborsOf :: (Applicative f, Num a, Num (f a), Ord (f a), Traversable f) => f a -> Set (f a)
neighborsOf = Set.fromList . flip map adjacencies . (+)

-- | All ways of removing one element from a list.
--   O(n²).
-- Agda.Utils.List
holes :: [a] -> [(a, [a])]
holes [] = []
holes (x : xs) = (x, xs) : map (second (x :)) (holes xs)

numDigits :: (Integral a) => a -> Int
numDigits n = truncate @Double (logBase 10 (fromIntegral n) + 1)

(<&&>) :: (Applicative f) => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

infixr 3 <&&> -- same as (&&)

(<||>) :: (Applicative f) => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

infixr 2 <||> -- same as (||)

(<.>) :: (Functor f) => (b -> c) -> (a -> f b) -> a -> f c
(f <.> g) a = f <$> g a

infixr 9 <.>
