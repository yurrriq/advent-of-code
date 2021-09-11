{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Util
  ( Frequencies,
    frequencies,
    frequenciesInt,
    maybeParseByteString,
    findFirstDup,
    hammingDistance,
    hammingSimilar,
    scan,
    count,
    snoc,
    wigglesum,
    fix',
    adjacencies,
    neighborsOf,
    holes,
  )
where

import Control.Arrow (second, (>>>))
import Control.Comonad.Store (experiment)
import Control.Lens (holesOf)
import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.Function (fix)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Trifecta (Parser, Result (..), parseByteString)

type Frequencies a = Map a Int

frequencies :: Ord a => [a] -> Frequencies a
frequencies = foldr go Map.empty
  where
    go k = Map.insertWith (+) k 1

frequenciesInt :: Foldable t => t Int -> IM.IntMap Int
frequenciesInt = foldr go IM.empty
  where
    go k = IM.insertWith (+) k 1

maybeParseByteString :: Parser a -> ByteString -> Maybe a
maybeParseByteString p =
  parseByteString p mempty >>> \case
    Failure _ -> Nothing
    Success res -> Just res

hammingDistance :: Eq a => [a] -> [a] -> Maybe Integer
hammingDistance (x : xs) (y : ys)
  | x /= y = (+ 1) <$> recur
  | otherwise = recur
  where
    recur = hammingDistance xs ys
hammingDistance [] [] = Just 0
hammingDistance _ _ = Nothing

hammingSimilar :: Eq a => Integer -> [a] -> [a] -> Bool
hammingSimilar n xs = maybe False (<= n) . hammingDistance xs

findFirstDup :: Ord a => [a] -> Maybe a
findFirstDup = go Set.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | x `Set.member` seen = Just x
      | otherwise = go (Set.insert x seen) xs

scan :: Monoid m => [m] -> [m]
scan = scanl mappend mempty

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

wigglesum :: Traversable t => (a -> [a]) -> t a -> [t a]
wigglesum wiggle = holesOf traverse >=> experiment wiggle

fix' :: Eq a => (a -> a) -> a -> a
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
