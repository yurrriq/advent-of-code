{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2021.Day04 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Lens (ifoldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (find)
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
   in case find isWinner marked of
        Nothing -> partOne (cs, marked)
        Just winner ->
          (c *) . sum . map fst . filter (not . snd) $
            IM.elems winner
partOne _ = error "No winner"

partTwo :: ([Int], [Board]) -> Int
partTwo = undefined

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
  [ [0 .. 4],
    [5 .. 9],
    [10 .. 14],
    [15 .. 19],
    [20 .. 24],
    [0, 5 .. 24],
    [1, 6 .. 24],
    [2, 7 .. 24],
    [3, 8 .. 24],
    [4, 9 .. 24]
  ]
