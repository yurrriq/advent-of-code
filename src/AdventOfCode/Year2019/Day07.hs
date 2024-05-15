{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AdventOfCode.Year2019.Day07 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Monad (forM, liftM2, when)
import Control.Monad.State (get, gets, lift, put)
import Control.Monad.Trans.State.Strict (StateT, execStateT)
import Data.Conduit (ConduitM, ConduitT, await, runConduit, yield, (.|))
import Data.Conduit.Lift (evalStateC)
import Data.FastDigits (digits)
import Data.List (permutations)
import Data.Vector (Vector, fromList, modify, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Trifecta (Parser, comma, integer, sepBy)

-- -------------------------------------------------------------------- [ Main ]

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    partOne input

getInput :: IO (Vector Int)
getInput = parseInput stack $(inputFilePath)

-- ------------------------------------------------------------------- [ Parts ]

partOne :: Vector Int -> IO ()
partOne prog =
  do
    let ampses = prepareAmps prog <$> permutations [0 .. 4]
    results <- forM ampses $ \[a, b, c, d, e] ->
      runConduit $
        yield 0
          .| a
          .| b
          .| c
          .| d
          .| e
          .| await'
    print (maximum results)

-- ------------------------------------------------------------------- [ Types ]

type Program = StateT ProgramState IO

type Stack = Vector Int

data ProgramState = ProgramState
  { _stack :: Stack,
    _pointer :: Int,
    _debug :: Bool
  }
  deriving (Eq, Show)

data Instruction
  = Add Value Value Int
  | Multiply Value Value Int
  | Set Value Int
  | Output Value
  | JumpIfTrue Value Value
  | JumpIfFalse Value Value
  | LessThan Value Value Int
  | Equals Value Value Int
  | End
  deriving (Eq, Show)

data Value
  = PositionMode !Int
  | ImmediateMode !Int
  deriving (Eq, Show)

-- ------------------------------------------------------------------ [ Parser ]

stack :: Parser (Vector Int)
stack = fromList . map fromInteger <$> (integer `sepBy` comma)

-- -------------------------------------------------------- [ Running Programs ]

runProgram :: ConduitT Int Int Program ()
runProgram =
  do
    opCode <- lift nextInt
    when (opCode /= 99) $
      do
        debugState
        instruction <- getInstruction (normalizeOpCode opCode)
        debugInstruction instruction
        runInstruction instruction
        debugState
        runProgram

debugMode :: Program ()
debugMode =
  do
    lift $ putStrLn "Enabling debug mode"
    state <- get
    put $ state {_debug = True}

debugInstruction :: Instruction -> ConduitT a b Program ()
debugInstruction ins =
  do
    st <- get
    when (_debug st) $
      lift (lift (print ins))

debugState :: ConduitT a b Program ()
debugState =
  do
    st <- get
    when (_debug st) $
      lift (lift (print st))

getInstruction :: [Int] -> ConduitT Int Int Program Instruction
getInstruction [1, 0, c, b] =
  lift $ Add <$> (mkValue c <$> nextInt) <*> (mkValue b <$> nextInt) <*> nextInt
getInstruction [2, 0, c, b] =
  lift $ Multiply <$> (mkValue c <$> nextInt) <*> (mkValue b <$> nextInt) <*> nextInt
getInstruction [3, 0, 0, 0] =
  do
    input <- await'
    lift $ Set (ImmediateMode input) <$> nextInt
getInstruction [4, 0, c, 0] =
  lift $ Output <$> (mkValue c <$> nextInt)
getInstruction [5, 0, c, b] =
  lift $ JumpIfTrue <$> (mkValue c <$> nextInt) <*> (mkValue b <$> nextInt)
getInstruction [6, 0, c, b] =
  lift $ JumpIfFalse <$> (mkValue c <$> nextInt) <*> (mkValue b <$> nextInt)
getInstruction [7, 0, c, b] =
  lift $ LessThan <$> (mkValue c <$> nextInt) <*> (mkValue b <$> nextInt) <*> nextInt
getInstruction [8, 0, c, b] =
  lift $ Equals <$> (mkValue c <$> nextInt) <*> (mkValue b <$> nextInt) <*> nextInt
getInstruction _ = error "Invalid instruction"

runInstruction :: Instruction -> ConduitT Int Int Program ()
runInstruction (Add vx vy dst) =
  lift $ flip setValue dst =<< (+) <$> handleValue vx <*> handleValue vy
runInstruction (Multiply vx vy dst) =
  lift $ flip setValue dst =<< (*) <$> handleValue vx <*> handleValue vy
runInstruction (Set vx dst) =
  lift $ flip setValue dst =<< handleValue vx
runInstruction (Output vx) =
  yield =<< lift (handleValue vx)
runInstruction (JumpIfTrue vx vy) =
  lift $ do
    x <- handleValue vx
    when (x /= 0) $
      jump vy
runInstruction (JumpIfFalse vx vy) =
  lift $ do
    x <- handleValue vx
    when (x == 0) $
      jump vy
runInstruction (LessThan vx vy dst) =
  lift $ do
    lt <- (<) <$> handleValue vx <*> handleValue vy
    if lt
      then setValue 1 dst
      else setValue 0 dst
runInstruction (Equals vx vy dst) =
  lift $ do
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

evalStack :: Stack -> ConduitT Int Int IO ()
evalStack st = evalStateC (initialState {_stack = st}) runProgram

evalStack' :: Stack -> ConduitT Int Int IO ()
evalStack' st =
  do
    state <- lift $ execStateT debugMode (initialState {_stack = st})
    evalStateC state runProgram

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
initialState = ProgramState {_stack = V.empty, _pointer = 0, _debug = False}

normalizeOpCode :: Int -> [Int]
normalizeOpCode d = take 4 $ digits 10 (fromIntegral d) ++ repeat 0

mkValue :: Int -> Int -> Value
mkValue 0 = PositionMode
mkValue 1 = ImmediateMode
mkValue _ = error "Invalid parameter mode"

prepareAmps :: Stack -> [Int] -> [ConduitM Int Int IO ()]
prepareAmps = map . flip prepareAmp

prepareAmp :: Int -> Stack -> ConduitM Int Int IO ()
prepareAmp phase prog = fuseAmp phase .| evalStack prog

fuseAmp :: Int -> ConduitT Int Int IO ()
fuseAmp phase =
  do
    input <- await'
    yield phase
    yield input

await' :: (Monad m) => ConduitT i o m i
await' = maybe (error "Missing input") pure =<< await
