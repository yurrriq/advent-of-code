{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AdventOfCode.Year2019.Day07 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Lens (makeLenses, use, (%=), (&), (+=), (.=), (.~))
import Control.Monad (forM, when)
import Control.Monad.State (get, lift)
import Control.Monad.Trans.State.Strict (StateT, execStateT)
import Data.Conduit (ConduitM, ConduitT, await, runConduit, yield, (.|))
import Data.Conduit.Lift (evalStateC)
import Data.FastDigits (digits)
import Data.List (permutations)
import Data.Vector (Vector, fromList, modify, (!))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Foreign.Marshal.Utils (fromBool)
import GHC.Generics (Generic)
import Text.Trifecta (Parser, comma, integer, sepBy)

-- ------------------------------------------------------------------- [ Types ]

type Program = StateT ProgramState IO

type Stack = Vector Int

data ProgramState = ProgramState
  { _stack :: Stack,
    _pointer :: Int,
    _debug :: Bool
  }
  deriving (Eq, Generic, Show)

makeLenses ''ProgramState

defaultProgramState :: ProgramState
defaultProgramState = ProgramState V.empty 0 False

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

-- -------------------------------------------------------------------- [ Main ]

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    partOne input

getInput :: IO (Vector Int)
getInput = parseInput parseStack $(inputFilePath)

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

-- ------------------------------------------------------------------ [ Parser ]

parseStack :: Parser (Vector Int)
parseStack = fromList . map fromInteger <$> (integer `sepBy` comma)

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
    debug .= True

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
    setValue (fromBool lt) dst
runInstruction (Equals vx vy dst) =
  lift $ do
    eq <- (==) <$> handleValue vx <*> handleValue vy
    setValue (fromBool eq) dst
runInstruction End = pure ()

jump :: Value -> Program ()
jump vy = (pointer .=) =<< handleValue vy

evalStack :: Stack -> ConduitT Int Int IO ()
evalStack st = evalStateC (defaultProgramState & stack .~ st) runProgram

evalStack' :: Stack -> ConduitT Int Int IO ()
evalStack' st =
  lift (execStateT debugMode (defaultProgramState & stack .~ st))
    >>= flip evalStateC runProgram

-- -------------------------------------------------- [ Manipulating the Stack ]

setValue :: Int -> Int -> Program ()
setValue x dst = stack %= modify (\v -> MV.write v dst x)

incrementPointer :: Program ()
incrementPointer = pointer += 1

nextInt :: Program Int
nextInt =
  do
    vx <- (!) <$> use stack <*> use pointer
    incrementPointer
    pure vx

handleValue :: Value -> Program Int
handleValue (PositionMode i) = use stack >>= flip V.indexM i
handleValue (ImmediateMode n) = pure n

-- -------------------------------------------------------- [ Helper Functions ]

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
