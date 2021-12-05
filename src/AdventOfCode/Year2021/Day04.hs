{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module AdventOfCode.Year2021.Day04 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMainMaybe, inputFilePath)
import Control.Arrow (first)
import Control.Lens (ifoldl')
import Data.Functor.Foldable (ana, hylo)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Safe.Foldable (maximumByMay)
import Text.Trifecta (Parser, commaSep, count, natural, some)

-- | A 'Board' is map from callable number to square.
-- Squares are numbered left to right and top to bottom.
type Board = IntMap Int

-- | A Bingo game is either ready to call a number, won with a score, or lost.
data Bingo
  = Call Bingo
  | Won Int
  | Lost
  deriving (Eq, Show)

makeBaseFunctor ''Bingo

-- Keep track of the numbers to call, and the squares marked.
data BingoState = BingoState
  { numbersToCall :: [Int],
    squaresMarked :: IntSet
  }
  deriving (Eq, Show)

main :: IO ()
main = $(defaultMainMaybe)

getInput :: IO ([Int], [Board])
getInput = parseInput input $(inputFilePath)
  where
    input = (,) <$> calls <*> some board

partOne :: ([Int], [Board]) -> Maybe Int
partOne (calls, boards) = step allGames
  where
    step = declareWinnerOrCallNext . callNumber
    declareWinnerOrCallNext = either id step
    callNumber = traverse $ \case
      Call games -> Right games
      Won score -> Left (Just score)
      Lost -> Left Nothing
    allGames = map (\b -> ana (bingoCoalg b) (BingoState calls IS.empty)) boards

partTwo :: ([Int], [Board]) -> Maybe Int
partTwo (calls, boards) =
  fmap snd
    . maximumByMay (comparing fst)
    . mapMaybe (\b -> hylo bingoAlg (bingoCoalg b) (BingoState calls IS.empty))
    $ boards

-- Count the number of called numbers.
bingoAlg :: BingoF (Maybe (Int, Int)) -> Maybe (Int, Int)
bingoAlg = \case
  CallF state -> first succ <$> state
  WonF score -> Just (1, score)
  LostF -> Nothing

bingoCoalg :: Board -> BingoState -> BingoF BingoState
bingoCoalg _ (BingoState [] _) = LostF
bingoCoalg board (BingoState (call : calls) marked) =
  case (`IS.insert` marked) <$> IM.lookup call board of
    Nothing -> CallF (BingoState calls marked)
    Just marked' ->
      if isWinner marked'
        then WonF (calculateScore call marked' board)
        else CallF (BingoState calls marked')

calculateScore :: Int -> IntSet -> Board -> Int
calculateScore lastCall marked =
  (lastCall *)
    . sum
    . IM.keys
    . IM.filter (`IS.notMember` marked)

isWinner :: IntSet -> Bool
isWinner marked = any (`IS.isSubsetOf` marked) runs

-- FIXME: This is gross and specific.
runs :: [IntSet]
runs =
  map IS.fromList . concat $
    [ [ [n * 5 .. n * 5 + 4],
        [n, n + 5 .. 24]
      ]
      | n <- [0 .. 4]
    ]

calls :: Parser [Int]
calls = commaSep (fromInteger <$> natural)

-- TODO: Support arbitrary (square) boards.
board :: Parser Board
board = mkBoard <$> count 5 (count 5 (fromInteger <$> natural))
  where
    mkBoard = ifoldl' go IM.empty . concat
    go i m n = IM.insert n i m
