{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2021.Day04 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Lens (ifoldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (find, maximumBy)
import Data.Ord (comparing)
import Text.Trifecta (Parser, commaSep, count, natural, some)

type Board = IntMap (Int, Bool)

main :: IO ()
main = $(defaultMain)

getInput :: IO ([Int], [Board])
getInput = parseInput input $(inputFilePath)
  where
    input = (,) <$> calls <*> some board

partOne :: ([Int], [Board]) -> Int
partOne (c : cs, bs) =
  let marked = map (markBoard c) bs
   in maybe
        (partOne (cs, marked))
        (scoreBoard c)
        (find isWinner marked)
partOne _ = error "No winner"

partTwo :: ([Int], [Board]) -> Int
partTwo (cs, bs) =
  let (c : _, b) =
        maximumBy
          (comparing (length . fst))
          (map (playToWin cs . ([],)) bs)
   in scoreBoard c b

playToWin :: [Int] -> ([Int], Board) -> ([Int], Board)
playToWin (c : cs) (seen, b) =
  let marked = markBoard c b
   in if isWinner marked
        then (c : seen, marked)
        else playToWin cs (c : seen, marked)
playToWin _ _ = error "Loser"

scoreBoard :: Int -> Board -> Int
scoreBoard c = (c *) . sum . map fst . filter (not . snd) . IM.elems

calls :: Parser [Int]
calls = commaSep (fromInteger <$> natural)

board :: Parser Board
board = mkBoard . concat <$> count 5 (count 5 (fromInteger <$> natural))

mkBoard :: [Int] -> Board
mkBoard = ifoldl' go IM.empty
  where
    go i m x = IM.insert i (x, False) m

markBoard :: Int -> Board -> Board
markBoard call = IM.map go
  where
    go v@(x, _)
      | call == x = (x, True)
      | otherwise = v

isWinner :: Board -> Bool
isWinner b = any (all (snd . (b IM.!))) runs

runs :: [[Int]]
runs =
  concat
    [ [ [n * 5 .. n * 5 + 4],
        [n, n + 5 .. 24]
      ]
      | n <- [0 .. 4]
    ]
