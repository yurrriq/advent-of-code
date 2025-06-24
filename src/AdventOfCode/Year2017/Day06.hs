module AdventOfCode.Year2017.Day06 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMainMaybe, inputFilePath)
import Control.Monad ((<=<))
import Data.Foldable (maximumBy)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (elemIndex, foldl', uncons)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Trifecta (natural, some)

main :: IO ()
main = $(defaultMainMaybe)

partOne :: IntMap Int -> Maybe Int
partOne = Just . Set.size . fst . reallocate

partTwo :: IntMap Int -> Maybe Int
partTwo = fmap (1 +) . uncurry elemIndex <=< (uncons . snd . reallocate)

getInput :: IO (IntMap Int)
getInput = parseInput parser $(inputFilePath)
  where
    parser = IntMap.fromList . zip [0 ..] <$> some (fromInteger <$> natural)

example :: IntMap Int
example = IntMap.fromList (zip [0 ..] [0, 2, 7, 0])

reallocate :: IntMap Int -> (Set (IntMap Int), [IntMap Int])
reallocate im = go (Set.singleton im) [im] im
  where
    go seen history before
      | Set.member after seen = (seen, after : history)
      | otherwise = go (Set.insert after seen) (after : history) after
      where
        after = step before

step :: IntMap Int -> IntMap Int
step im =
  foldl' go (IntMap.insert k 0 im) $
    take v (drop (k + 1) (cycle [0 .. IntMap.size im - 1]))
  where
    go im' i = IntMap.update (Just . (+ 1)) i im'
    (k, v) = next im

next :: IntMap Int -> (Int, Int)
next = maximumBy comparingValue . IntMap.assocs
  where
    comparingValue (k, v) (k', v') =
      case compare v v' of
        EQ -> compare k' k
        ordering -> ordering
