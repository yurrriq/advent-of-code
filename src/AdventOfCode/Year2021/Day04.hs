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
import Data.Functor.Foldable (Base, Corecursive (embed), Recursive (project), ana, hylo)
-- import Data.Functor.Foldable.TH (makeBaseFunctor)
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

-- FIXME: For some reason Nix doesn't seem to like this, so splice in place.
-- START: makeBasefunctor ''Bingo
data BingoF r
  = CallF r
  | WonF Int
  | LostF
  deriving (Functor, Foldable, Traversable)

type instance Base Bingo = BingoF

instance Recursive Bingo where
  project (Call x) = CallF x
  project (Won x) = WonF x
  project Lost = LostF

instance Corecursive Bingo where
  embed (CallF x) = Call x
  embed (WonF x) = Won x
  embed LostF = Lost

-- END: makeBaseFunctor ''Bingo

-- | Keep track of the list of numbers to call, and the set of squares marked.
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
    input = (,) <$> calls <*> some (board 5)

partOne :: ([Int], [Board]) -> Maybe Int
partOne (calls, boards) = step allGames
  where
    step = declareWinnerOrCallNext . callNumber
    declareWinnerOrCallNext = either id step
    callNumber = traverse $ \case
      Call games -> Right games
      Won score -> Left (Just score)
      Lost -> Left Nothing
    allGames = map (flip ana (BingoState calls IS.empty) . bingoCoalg 5) boards

partTwo :: ([Int], [Board]) -> Maybe Int
partTwo (calls, boards) =
  fmap snd
    . maximumByMay (comparing fst)
    . mapMaybe (flip (hylo bingoAlg . bingoCoalg 5) (BingoState calls IS.empty))
    $ boards

-- | Count the number of called numbers.
bingoAlg :: BingoF (Maybe (Int, Int)) -> Maybe (Int, Int)
bingoAlg = \case
  CallF state -> first succ <$> state
  WonF score -> Just (1, score)
  LostF -> Nothing

bingoCoalg :: Int -> Board -> BingoState -> BingoF BingoState
bingoCoalg _ _ (BingoState [] _) = LostF
bingoCoalg k board (BingoState (call : calls) marked) =
  case (`IS.insert` marked) <$> IM.lookup call board of
    Nothing -> CallF (BingoState calls marked)
    Just marked' ->
      if isWinner k marked'
        then WonF (calculateScore call marked' board)
        else CallF (BingoState calls marked')

calculateScore :: Int -> IntSet -> Board -> Int
calculateScore lastCall marked =
  (lastCall *)
    . sum
    . IM.keys
    . IM.filter (`IS.notMember` marked)

isWinner :: Int -> IntSet -> Bool
isWinner k = flip any (runs k) . flip IS.isSubsetOf

runs :: Int -> [IntSet]
runs k =
  map IS.fromList . concat $
    [ [ [n * k .. n * k + (k - 1)],
        [n, n + k .. k * k - 1]
      ]
      | n <- [0 .. k - 1]
    ]

calls :: Parser [Int]
calls = commaSep (fromInteger <$> natural)

board :: Int -> Parser Board
board k = mkBoard <$> count k (count k (fromInteger <$> natural))
  where
    mkBoard = ifoldl' go IM.empty . concat
    go i m n = IM.insert n i m
