module AdventOfCode.Year2020.Day08 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (wigglesum)
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad.State (State, evalState, get, gets)
import Data.Either (rights)
import Text.Parser.Token.Highlight (Highlight (..))
import Text.Trifecta ((<?>), Parser, highlight, integer, some, symbol)

type Instruction = (Operation, Int)

data Operation
  = ACC
  | JMP
  | NOP
  deriving (Eq, Show)

data ProgState
  = ProgState
      { _cursor :: Int,
        _history :: [Int],
        _accumulator :: Int
      }
  deriving (Eq, Show)

makeLenses ''ProgState

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne input
    putStr "Part Two: "
    print $ partTwo input

getInput :: IO [Instruction]
getInput = parseInput (some instruction) $(inputFilePath)

partOne :: [Instruction] -> Int
partOne prog = answer where Left answer = evalState (program prog) initialState

partTwo :: [Instruction] -> Int
partTwo =
  head . rights
    . map (flip evalState initialState . program)
    . wigglesum (_1 wiggle)
  where
    wiggle ACC = []
    wiggle JMP = [NOP]
    wiggle NOP = [JMP]

instruction :: Parser Instruction
instruction = (,) <$> operation <*> (fromInteger <$> integer)
  where
    operation =
      highlight Operator $
        (symbol "acc" *> pure ACC <?> "accumulator")
          <|> (symbol "jmp" *> pure JMP <?> "jump")
          <|> (symbol "nop" *> pure NOP <?> "no operation")

initialState :: ProgState
initialState = ProgState 0 [] 0

program :: [Instruction] -> State ProgState (Either Int Int)
program prog =
  do
    ProgState n seen acc <- get
    if n == length prog
      then pure (Right acc)
      else
        if n `elem` seen
          then pure (Left acc)
          else runInstruction (prog !! n) *> program prog

runInstruction :: Instruction -> State ProgState ()
runInstruction (ACC, x) = moveCursor 1 *> (accumulator += x)
runInstruction (JMP, x) = moveCursor x
runInstruction (NOP, _) = moveCursor 1

moveCursor :: Int -> State ProgState ()
moveCursor x =
  do
    n <- gets (view cursor)
    history %= (n :)
    cursor += x
