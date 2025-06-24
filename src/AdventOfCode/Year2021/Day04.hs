{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module AdventOfCode.Year2021.Day04 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMainMaybe, inputFilePath)
import Control.Arrow (first)
import Control.Lens (ifoldl')
import Data.Functor.Foldable (ana, hylo)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
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
  deriving (Eq, Ord, Show)

makeBaseFunctor ''Bingo

main :: IO ()
main = $(defaultMainMaybe)

-- | The input is a list of numbers to call and list of bingo boards.
getInput :: IO ([Int], [Board])
getInput = parseInput callsAndBoards $(inputFilePath)

callsAndBoards :: Parser ([Int], [Board])
callsAndBoards = (,) <$> calls <*> some (board 5)
  where
    calls = commaSep (fromInteger <$> natural)
    board k = mkBoard <$> count k (count k (fromInteger <$> natural))
      where
        mkBoard = ifoldl' go IM.empty . concat
        go i m n = IM.insert n i m

-- | Given a list of called numbers and a list of boards, compute the score of
-- the 'Board' that wins first.
partOne :: ([Int], [Board]) -> Maybe Int
partOne (_, []) = Nothing
partOne (calls, boards) =
  trampoline (traverse runGame)
    . map (flip ana (calls, IS.empty) . bingoCoalg 5)
    $ boards

-- | Given a list of called numbers and a list of boards, compute the score of
-- the 'Board' that wins last.
partTwo :: ([Int], [Board]) -> Maybe Int
partTwo (calls, boards) =
  fmap snd
    . maximumByMay (comparing fst)
    . mapMaybe (flip (hylo bingoAlg . bingoCoalg 5) (calls, IS.empty))
    $ boards

-- | A bingo coalgebra with a carrier type that tracks the numbers remaining to
-- be called and the set of marked numbers.
bingoCoalg :: Int -> Board -> ([Int], IntSet) -> BingoF ([Int], IntSet)
bingoCoalg _ _ ([], _) = LostF
bingoCoalg k board (call : calls, alreadyMarked) =
  maybe callNext (checkIfWon . mark) (IM.lookup call board)
  where
    mark square = IS.insert square alreadyMarked
    checkIfWon marked
      | isWinner k marked = WonF (calculateScore call marked board)
      | otherwise = CallF (calls, marked)
    callNext = CallF (calls, alreadyMarked)

runGame :: Bingo -> Either (Maybe Int) Bingo
runGame = \case
  Call game -> Right game
  Won score -> Left (Just score)
  Lost -> Left Nothing

-- | A bingo algebra with a carrier type that counts the number of called
-- numbers and returns the final score in case of a winning game.
bingoAlg :: BingoF (Maybe (Int, Int)) -> Maybe (Int, Int)
bingoAlg = \case
  CallF state -> first succ <$> state
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
  map IS.fromList . concat $
    [ [ [n * k .. n * k + (k - 1)],
        [n, n + k .. k * k - 1]
      ]
      | n <- [0 .. k - 1]
    ]

-- Agda.Utils.Function.trampoline
trampoline :: (a -> Either b a) -> a -> b
trampoline f = loop
  where
    loop a = either id loop (f a)

example :: IO ([Int], [Board])
example =
  parseString callsAndBoards . unlines $
    [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
      "",
      "22 13 17 11  0",
      " 8  2 23  4 24",
      "21  9 14 16  7",
      " 6 10  3 18  5",
      " 1 12 20 15 19",
      "",
      " 3 15  0  2 22",
      " 9 18 13 17  5",
      "19  8  7 25 23",
      "20 11 10 24  4",
      "14 21 16 12  6",
      "",
      "14 21 17 24  4",
      "10 16 15  9 19",
      "18  8 23 26 20",
      "22 11 13  6  5",
      " 2  0 12  3  7"
    ]
