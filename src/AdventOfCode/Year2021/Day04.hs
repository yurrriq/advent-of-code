{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
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

-- | A Bingo game has three potential states.
data Bingo
  = -- | Ready to call a number.
    Call Bingo
  | -- | Won with a score.
    Won Int
  | -- | Lost.
    Lost
  deriving (Eq, Show)

makeBaseFunctor ''Bingo

-- | Keep track of the list of numbers to call, and the set of squares marked.
data BingoState = BingoState
  { numbersToCall :: [Int],
    squaresMarked :: IntSet
  }
  deriving (Eq, Show)

main :: IO ()
main = $(defaultMainMaybe)

-- | The input is a list of numbers to call and list of bingo boards.
getInput :: IO ([Int], [Board])
getInput = parseInput input $(inputFilePath)
  where
    input = (,) <$> calls <*> some (board 5)

-- | Given a list of called numbers and a list of boards, compute the score of
-- the 'Board' that wins first.
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

-- | Given a list of called numbers and a list of boar ds, compute the score of
-- the 'Board' that wins last.
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
bingoCoalg k board (BingoState (call : calls) alreadyMarked) =
  maybe callNext (checkIfWon . mark) (IM.lookup call board)
  where
    mark square = IS.insert square alreadyMarked
    checkIfWon marked
      | isWinner k marked = WonF (calculateScore call marked board)
      | otherwise = CallF (BingoState calls marked)
    callNext = CallF (BingoState calls alreadyMarked)

-- | Given the last called number, a set of marked squares, and a board,
-- calculate the score, i.e. the product of the last called number and the sum
-- of the unmarked numbers on the board.
calculateScore :: Int -> IntSet -> Board -> Int
calculateScore lastCall marked =
  (lastCall *)
    . sum
    . IM.keys
    . IM.filter (`IS.notMember` marked)

-- | Given a size @k@, and a set of marked squares, determine if all squares in
-- any row or any column of a \(k \times k\) 'Board' are marked.
isWinner :: Int -> IntSet -> Bool
isWinner k marked = any (`IS.isSubsetOf` marked) (runs k)

-- | The complete list of winning sets of square numbers on a \(k \times k\)
-- 'Board'.
runs :: Int -> [IntSet]
runs k =
  map IS.fromList . concat $
    [ [ [n * k .. n * k + (k - 1)],
        [n, n + k .. k * k - 1]
      ]
      | n <- [0 .. k - 1]
    ]

-- | A list of numbers to call.
calls :: Parser [Int]
calls = commaSep (fromInteger <$> natural)

-- | A \(k \times k\) bingo 'Board'.
board :: Int -> Parser Board
board k = mkBoard <$> count k (count k (fromInteger <$> natural))
  where
    mkBoard = ifoldl' go IM.empty . concat
    go i m n = IM.insert n i m
