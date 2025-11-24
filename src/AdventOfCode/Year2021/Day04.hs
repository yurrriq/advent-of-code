{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2021.Day04 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (maybeFail)
import Control.Lens (ifoldl', makeLenses, views)
import Data.Functor.Foldable (ana, hylo)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Relude
import Safe.Foldable (maximumByMay)
import Text.Trifecta (Parser, commaSep, count, natural)

-- | A 'Board' is map from callable number to square.
-- Squares are numbered left to right and top to bottom.
type Board = IntMap Int

-- | A Bingo game has three potential states.
data Bingo
  = -- | Ready to call a number.
    Call !Bingo
  | -- | Won with a score.
    Won !Int
  | -- | Lost.
    Lost
  deriving (Eq, Generic, Ord, Show)

makeBaseFunctor ''Bingo

data BingoState
  = BingoState
  { _calls :: ![Int],
    _boards :: ![Board]
  }
  deriving (Eq, Generic, Show)

makeLenses ''BingoState

type CallState = ([Int], IntSet)

main :: IO ()
main = $(defaultMainPuzzle)

-- | The input is a list of numbers to call and list of bingo boards.
getInput :: IO BingoState
getInput = parseInputAoC 2021 4 callsAndBoards

callsAndBoards :: Parser BingoState
callsAndBoards = BingoState <$> commaSep int <*> some (board 5)
  where
    int = fromInteger <$> natural
    board k = mkBoard <$> count k (count k int)
      where
        mkBoard = ifoldl' go IM.empty . concat
        go i m n = IM.insert n i m

getExample :: IO BingoState
getExample = parseString callsAndBoards example

example :: String
example =
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
  \\n\
  \22 13 17 11  0\n\
  \ 8  2 23  4 24\n\
  \21  9 14 16  7\n\
  \ 6 10  3 18  5\n\
  \ 1 12 20 15 19\n\
  \\n\
  \ 3 15  0  2 22\n\
  \ 9 18 13 17  5\n\
  \19  8  7 25 23\n\
  \20 11 10 24  4\n\
  \14 21 16 12  6\n\
  \\n\
  \14 21 17 24  4\n\
  \10 16 15  9 19\n\
  \18  8 23 26 20\n\
  \22 11 13  6  5\n\
  \ 2  0 12  3  7\n"

-- | Given a list of called numbers and a list of boards, compute the score of
-- the 'Board' that wins first.
partOne :: SimplePuzzle BingoState Int
partOne =
  solve ana
    $ trampoline
    $ traverse runGame

-- | Given a list of called numbers and a list of boards, compute the score of
-- the 'Board' that wins last.
partTwo :: SimplePuzzle BingoState Int
partTwo =
  solve (hylo bingoAlg)
    $ catMaybes
    >>> maximumByMay (comparing fst)
    >>> fmap snd

solve ::
  ((CallState -> BingoF CallState) -> CallState -> a) ->
  ([a] -> Maybe b) ->
  SimplePuzzle BingoState b
solve process findScore = do
  views calls (,IS.empty) >>= \callState ->
    findScore
      . map (flip process callState . bingoCoalg 5)
      & views boards
      >>= maybeFail "no winners"

-- | A bingo coalgebra with a carrier type that tracks the numbers remaining to
-- be called and the set of marked numbers.
bingoCoalg :: Int -> Board -> CallState -> BingoF CallState
bingoCoalg _ _ ([], _) = LostF
bingoCoalg k board (number : numbers, alreadyMarked) =
  maybe callNext (checkIfWon . mark) (IM.lookup number board)
  where
    mark square = IS.insert square alreadyMarked
    checkIfWon marked
      | isWinner k marked = WonF (calculateScore number marked board)
      | otherwise = CallF (numbers, marked)
    callNext = CallF (numbers, alreadyMarked)

runGame :: Bingo -> Either (Maybe Int) Bingo
runGame = \case
  Call game -> Right game
  Won score -> Left (Just score)
  Lost -> Left Nothing

-- | A bingo algebra with a carrier type that counts the number of called
-- numbers and returns the final score in case of a winning game.
bingoAlg :: BingoF (Maybe (Int, Int)) -> Maybe (Int, Int)
bingoAlg = \case
  CallF carrier -> first (+ 1) <$> carrier
  WonF score -> Just (1, score)
  LostF -> Nothing

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
  map IS.fromList
    . concat
    $ [ [ [n * k .. n * k + (k - 1)],
          [n, n + k .. k * k - 1]
        ]
      | n <- [0 .. k - 1]
      ]

-- Agda.Utils.Function.trampoline
trampoline :: (a -> Either b a) -> a -> b
trampoline f = loop
  where
    loop a = either id loop (f a)
