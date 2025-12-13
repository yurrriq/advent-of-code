{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2019.Day05
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import Control.Lens (makeLenses, use, (%=), (.=), (<<+=), (<~))
import Data.FastDigits (digits)
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Generic.Data (GenericProduct (..))
import Relude
import Text.Trifecta (Parser, comma, integer, sepBy)

-- ------------------------------------------------------------------- [ Types ]

type Stack = Vector Int

data ProgramState' a = ProgramState'
  { _programStack :: Stack,
    _programPointer :: a
  }
  deriving (Eq, Generic, Show)
  deriving
    (Semigroup, Monoid)
    via (GenericProduct (ProgramState' (Sum a)))

makeLenses ''ProgramState'

type ProgramState = ProgramState' Int

data Instruction
  = Add Value Value Int
  | Multiply Value Value Int
  | Set Value Int
  | Print Value
  | JumpIfTrue Value Value
  | JumpIfFalse Value Value
  | LessThan Value Value Int
  | Equals Value Value Int
  | End
  deriving (Eq, Show)

data Value
  = PositionMode Int
  | ImmediateMode Int
  deriving (Eq, Generic, Show)

type Program = Puzzle (Stack, Int) ProgramState

-- -------------------------------------------------------------------- [ Main ]

main :: IO ()
main = do
  input <- getInput
  putStr "Part One: " *> evaluatingPuzzle partOne input
  putStr "Part Two: " *> evaluatingPuzzle partTwo input

getInput :: IO (Stack, (Int, Int))
getInput = (,(1, 5)) <$> parseInputAoC 2019 5 stack

-- ------------------------------------------------------------------- [ Parts ]

partOne :: Puzzle (Stack, (Int, Int)) ProgramState ()
partOne = Puzzle $ withReaderT (second fst) (runPuzzle evalStack)

partTwo :: Puzzle (Stack, (Int, Int)) ProgramState ()
partTwo = Puzzle $ withReaderT (second snd) (runPuzzle evalStack)

-- ------------------------------------------------------------------ [ Parser ]

stack :: Parser Stack
stack = V.fromList <$> int `sepBy` comma

int :: Parser Int
int = fromInteger <$> integer

-- -------------------------------------------------------- [ Running Programs ]
-- TODO: ContT

runProgram :: Puzzle (Stack, Int) ProgramState ()
runProgram = do
  opCode <- nextInt
  when (opCode /= 99) do
    let normalized = normalizeOpCode opCode
    instruction <- getInstruction normalized
    runInstruction instruction
    runProgram

getInstruction :: [Int] -> Program Instruction
getInstruction [1, 0, c, b] =
  (Add . mkValue c <$> nextInt) <*> (mkValue b <$> nextInt) <*> nextInt
getInstruction [2, 0, c, b] =
  (Multiply . mkValue c <$> nextInt) <*> (mkValue b <$> nextInt) <*> nextInt
getInstruction [3, 0, 0, 0] =
  Set <$> asks (ImmediateMode . snd) <*> nextInt
getInstruction [4, 0, c, 0] =
  Print . mkValue c <$> nextInt
getInstruction [5, 0, c, b] =
  (JumpIfTrue . mkValue c <$> nextInt) <*> (mkValue b <$> nextInt)
getInstruction [6, 0, c, b] =
  (JumpIfFalse . mkValue c <$> nextInt) <*> (mkValue b <$> nextInt)
getInstruction [7, 0, c, b] =
  (LessThan . mkValue c <$> nextInt) <*> (mkValue b <$> nextInt) <*> nextInt
getInstruction [8, 0, c, b] =
  (Equals . mkValue c <$> nextInt) <*> (mkValue b <$> nextInt) <*> nextInt
getInstruction ins = fail $ "Invalid instruction: " <> show ins

runInstruction :: Instruction -> Program ()
runInstruction (Add vx vy dst) =
  flip setValue dst =<< (+) <$> handleValue vx <*> handleValue vy
runInstruction (Multiply vx vy dst) =
  flip setValue dst =<< (*) <$> handleValue vx <*> handleValue vy
runInstruction (Set vx dst) =
  flip setValue dst =<< handleValue vx
runInstruction (Print vx) = do
  x <- handleValue vx
  when (x /= 0)
    $ print x
runInstruction (JumpIfTrue vx vy) = do
  x <- handleValue vx
  when (x /= 0)
    $ jump vy
runInstruction (JumpIfFalse vx vy) = do
  x <- handleValue vx
  when (x == 0)
    $ jump vy
runInstruction (LessThan vx vy dst) = do
  lt <- (<) <$> handleValue vx <*> handleValue vy
  if lt
    then setValue 1 dst
    else setValue 0 dst
runInstruction (Equals vx vy dst) = do
  eq <- (==) <$> handleValue vx <*> handleValue vy
  if eq
    then setValue 1 dst
    else setValue 0 dst
runInstruction End = pure ()

jump :: Value -> Program ()
jump vy = do
  y <- handleValue vy
  programPointer .= y

evalStack :: Program ()
evalStack = do
  programPointer .= 0
  programStack <~ asks fst
  runProgram

-- -------------------------------------------------- [ Manipulating the Stack ]

setValue :: Int -> Int -> Program ()
setValue x dst = programStack %= V.modify (\v -> MV.write v dst x)

nextInt :: Program Int
nextInt = liftA2 (!) (use programStack) (programPointer <<+= 1)

handleValue :: Value -> Program Int
handleValue (PositionMode i) = use programStack >>= flip V.indexM i
handleValue (ImmediateMode n) = pure n

-- -------------------------------------------------------- [ Helper Functions ]

normalizeOpCode :: Int -> [Int]
normalizeOpCode d = take 4 $ digits 10 (fromIntegral d) ++ repeat 0

mkValue :: Int -> Int -> Value
mkValue 0 = PositionMode
mkValue 1 = ImmediateMode
mkValue _ = error "Invalid parameter mode"
