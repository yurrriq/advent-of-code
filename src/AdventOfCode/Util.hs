{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Util
  ( Frequencies,
    frequencies,
    maybeParseByteString,
    commonElems,
    findFirstDup,
    hammingDistance,
    hammingSimilar,
    scan,
  )
where

import Control.Category ((>>>))
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable (..))
import Text.Trifecta (Parser, Result (..), parseByteString)

type Frequencies a = HM.HashMap a Integer

frequencies :: (Eq a, Hashable a) => [a] -> Frequencies a
frequencies = foldr go HM.empty
  where
    go :: (Eq a, Hashable a) => a -> Frequencies a -> Frequencies a
    go k = HM.insertWith (+) k 1

maybeParseByteString :: Parser a -> ByteString -> Maybe a
maybeParseByteString p = parseByteString p mempty >>> \case
  Failure _ -> Nothing
  Success res -> Just res

commonElems :: (Eq a) => [a] -> [a] -> Maybe [a]
commonElems (x : xs) (y : ys)
  | x == y = Just [x] <> recur
  | otherwise = recur
  where
    recur = commonElems xs ys
commonElems _ _ = Nothing

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

findFirstDup :: (Eq a, Hashable a) => [a] -> Maybe a
findFirstDup = go HS.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | x `HS.member` seen = Just x
      | otherwise = go (HS.insert x seen) xs

scan :: Monoid m => [m] -> [m]
scan = scanl mappend mempty
