module AdventOfCode.Year2019.Day05
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Monad (liftM2, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT, get, gets, put)
import Data.FastDigits (digits)
import Data.Vector (Vector, fromList, modify, (!))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Text.Trifecta (Parser, comma, integer, sepBy)

-- -------------------------------------------------------------------- [ Main ]

main :: IO ()
main =
  do
    input <- parseInput stack $(inputFilePath)
    putStr "Part One> "
    partOne input
    putStr "Part Two> "
    partTwo input

-- ------------------------------------------------------------------- [ Parts ]

partOne :: Vector Int -> IO ()
partOne = evalStack

partTwo :: Vector Int -> IO ()
partTwo = partOne

-- ------------------------------------------------------------------- [ Types ]

type Program = StateT ProgramState IO

type Stack = Vector Int

data ProgramState = ProgramState
  { _stack :: Stack,
    _pointer :: Int
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- ------------------------------------------------------------------ [ Parser ]

stack :: Parser (Vector Int)
stack = fromList . map fromInteger <$> (integer `sepBy` comma)

-- -------------------------------------------------------- [ Running Programs ]
-- TODO: ContT

runProgram :: Program ()
runProgram =
  do
    opCode <- nextInt
    if opCode == 99
      then pure ()
      else do
        instruction <- getInstruction (normalizeOpCode opCode)
        runInstruction instruction
        runProgram

getInstruction :: [Int] -> Program Instruction
getInstruction [1, 0, c, b] =
  Add <$> (mkValue c <$> nextInt) <*> (mkValue b <$> nextInt) <*> nextInt
getInstruction [2, 0, c, b] =
  Multiply <$> (mkValue c <$> nextInt) <*> (mkValue b <$> nextInt) <*> nextInt
getInstruction [3, 0, 0, 0] =
  Set <$> liftIO (ImmediateMode . read <$> getLine) <*> nextInt
getInstruction [4, 0, c, 0] =
  Print <$> (mkValue c <$> nextInt)
getInstruction [5, 0, c, b] =
  JumpIfTrue <$> (mkValue c <$> nextInt) <*> (mkValue b <$> nextInt)
getInstruction [6, 0, c, b] =
  JumpIfFalse <$> (mkValue c <$> nextInt) <*> (mkValue b <$> nextInt)
getInstruction [7, 0, c, b] =
  LessThan <$> (mkValue c <$> nextInt) <*> (mkValue b <$> nextInt) <*> nextInt
getInstruction [8, 0, c, b] =
  Equals <$> (mkValue c <$> nextInt) <*> (mkValue b <$> nextInt) <*> nextInt
getInstruction _ = error "Invalid instruction"

runInstruction :: Instruction -> Program ()
runInstruction (Add vx vy dst) =
  flip setValue dst =<< (+) <$> handleValue vx <*> handleValue vy
runInstruction (Multiply vx vy dst) =
  flip setValue dst =<< (*) <$> handleValue vx <*> handleValue vy
runInstruction (Set vx dst) =
  flip setValue dst =<< handleValue vx
runInstruction (Print vx) =
  do
    x <- handleValue vx
    when (x /= 0) $
      liftIO $
        print x
runInstruction (JumpIfTrue vx vy) =
  do
    x <- handleValue vx
    when (x /= 0) $
      jump vy
runInstruction (JumpIfFalse vx vy) =
  do
    x <- handleValue vx
    when (x == 0) $
      jump vy
runInstruction (LessThan vx vy dst) =
  do
    lt <- (<) <$> handleValue vx <*> handleValue vy
    if lt
      then setValue 1 dst
      else setValue 0 dst
runInstruction (Equals vx vy dst) =
  do
    eq <- (==) <$> handleValue vx <*> handleValue vy
    if eq
      then setValue 1 dst
      else setValue 0 dst
runInstruction End = pure ()

jump :: Value -> Program ()
jump vy =
  do
    state <- get
    y <- handleValue vy
    put $ state {_pointer = y}

evalStack :: Stack -> IO ()
evalStack st = evalStateT runProgram (initialState {_stack = st})

-- -------------------------------------------------- [ Manipulating the Stack ]
-- TODO: Lenses

setValue :: Int -> Int -> Program ()
setValue x dst =
  do
    state <- get
    put $ state {_stack = modify (\v -> MV.write v dst x) (_stack state)}

incrementPointer :: Program ()
incrementPointer =
  do
    state <- get
    put $ state {_pointer = _pointer state + 1}

nextInt :: Program Int
nextInt =
  do
    vx <- gets (liftM2 (!) _stack _pointer)
    incrementPointer
    pure vx

handleValue :: Value -> Program Int
handleValue (PositionMode i) = flip V.indexM i . _stack =<< get
handleValue (ImmediateMode n) = pure n

-- -------------------------------------------------------- [ Helper Functions ]

initialState :: ProgramState
initialState = ProgramState {_stack = V.empty, _pointer = 0}

normalizeOpCode :: Int -> [Int]
normalizeOpCode d = take 4 $ digits 10 (fromIntegral d) ++ repeat 0

mkValue :: Int -> Int -> Value
mkValue 0 = PositionMode
mkValue 1 = ImmediateMode
mkValue _ = error "Invalid parameter mode"
