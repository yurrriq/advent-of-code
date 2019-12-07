module Data.AOC19.Day02 where

import           Control.Arrow           (first, (>>>))
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Digits             (digitsRev)
import           Data.List               (find)
import           Data.Vector             (Vector, fromList, modify, slice,
                                          toList, (!))
import qualified Data.Vector             as V
import           Data.Vector.Mutable     (write)
import qualified Data.Vector.Mutable     as MV
import           Text.Trifecta           (Parser, Result (..), comma, natural,
                                          parseFromFile, parseString, sepBy)

data OpCode
  = ADD
  | MUL
  | SET
  | PRN
  | END
  deriving (Eq, Show)


data Value
  = PositionMode Int
  | ImmediateMode Int
  deriving (Eq, Show)


-- data Instruction
--   = Add


program :: Parser (Vector Int)
program = fromList . map fromInteger <$> (natural `sepBy` comma)


runProgram :: Vector Int -> IO (Vector Int)
runProgram = runProgram' 0


runProgram' :: Int -> Vector Int -> IO (Vector Int)
runProgram' n state = step (digitsRev 10 instruction)
  where
    instruction :: Int
    instruction = state ! n

    step :: [Int] -> IO (Vector Int)
    step [1] = step [1,0,0,0]
    step [2] = step [2,0,0,0]
    step [1,0,c,b] =
      do let params = zipWith ($) (mkValue <$> [c,b,0]) $
                      toList (slice (n+1) 3 state)
         runProgram' (n+4) =<< runInstruction ADD params state
    step [2,0,c,b] =
      let params = zipWith ($) (mkValue <$> [c,b,0]) $
                   toList (slice (n+1) 3 state) in
        runProgram' (n+4) =<< runInstruction MUL params state
    step [3]       = do x <- read <$> getLine
                        runProgram' (n+2) =<< pure (modify (\v -> MV.write v (state ! (n+1)) x) state)
    step [4]       = do print (state ! (state ! (n+1)))
                        runProgram' (n+2) state
    step [9,9]     = pure state
    step _         = error $ "Invalid instruction: " ++ show instruction


mkValue :: Int -> Int -> Value
mkValue 0 = PositionMode
mkValue 1 = ImmediateMode
mkValue _ = error "Invalid parameter mode"


runInstruction :: OpCode -> [Value] -> Vector Int -> IO (Vector Int)
runInstruction ADD [vx, vy, PositionMode dst] state =
  do x <- handleValue vx state
     y <- handleValue vy state
     pure $ modify (\v -> MV.write v dst (x + y)) state
runInstruction MUL [vx, vy, PositionMode dst] state =
  do x <- handleValue vx state
     y <- handleValue vy state
     pure $ modify (\v -> MV.write v dst (x * y)) state
runInstruction _ _ state = fail "Invalid instruction"


handleValue :: Value -> Vector Int -> IO Int
handleValue (PositionMode i)  = flip V.indexM i
handleValue (ImmediateMode n) = const (pure n)

-- example1 :: Vector Int
-- example1 =
--     case parseString program mempty "1,9,10,3,2,3,11,0,99,30,40,50" of
--       Success prog   -> prog
--       Failure reason -> error (show reason)
