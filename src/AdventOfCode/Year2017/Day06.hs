module AdventOfCode.Year2017.Day06 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Year2015.Day24 (bruteForce)
import Data.Foldable (maximumBy)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (elemIndex, foldl', unfoldr)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Text.Trifecta (natural, some)

main :: IO ()
main = $(defaultMain)

partOne :: IntMap Int -> Int
partOne = (+ 1) . length . unfoldr go . ((,) =<< Set.singleton)
  where
    go (seen, before)
      | Set.member after seen = Nothing
      | otherwise = Just (seen', (seen', after))
      where
        after = step before
        seen' = Set.insert after seen

partTwo :: IntMap Int -> Int
partTwo im = go (Set.singleton im) [im] im
  where
    go seen history before
      | Set.member after seen = 1 + fromJust (elemIndex after history)
      | otherwise = go (Set.insert after seen) (after : history) after
      where
        after = step before

getInput :: IO (IntMap Int)
getInput = parseInput parser $(inputFilePath)
  where
    parser = IntMap.fromList . zip [0 ..] <$> some (fromInteger <$> natural)

example :: IntMap Int
example = IntMap.fromList (zip [0 ..] [0, 2, 7, 0])

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
