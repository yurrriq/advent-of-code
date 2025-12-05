{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2017.Day06 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (maybeFail)
import Data.IntMap.Strict qualified as IntMap
import Data.List (elemIndex)
import Data.Semigroup (Max (..))
import Data.Set qualified as Set
import Relude
import Text.Trifecta (natural)

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle (IntMap Int) Int
partOne = asks (Set.size . fst . reallocate)

partTwo :: SimplePuzzle (IntMap Int) Int
partTwo =
  asks (snd . reallocate)
    >>= (uncons >>> maybeFail "empty list")
    >>= (uncurry elemIndex >>> maybeFail "not found")
    <&> (+ 1)

getExample :: IO (IntMap Int)
getExample = pure $ IntMap.fromList (zip [0 ..] [0, 2, 7, 0])

getInput :: IO (IntMap Int)
getInput =
  parseInputAoC 2017 6
    $ IntMap.fromList
    . zip [0 ..]
    <$> some (fromInteger <$> natural)

reallocate :: IntMap Int -> (Set (IntMap Int), [IntMap Int])
reallocate = join (liftA2 go Set.singleton return)
  where
    go seen history before
      | Set.member after seen = (seen, after : history)
      | otherwise = go (Set.insert after seen) (after : history) after
      where
        after = next before

next :: IntMap Int -> IntMap Int
next before =
  maxValMinKey before & \(k, v) ->
    take v (drop (k + 1) (cycle (IntMap.keys before)))
      & foldl' (flip (IntMap.alter (fmap (+ 1)))) (IntMap.insert k 0 before)

maxValMinKey :: IntMap Int -> (Int, Int)
maxValMinKey =
  IntMap.foldMapWithKey go
    >>> getMax
    >>> second getDown
    >>> swap
  where
    go k v = Max (v, Down k)
