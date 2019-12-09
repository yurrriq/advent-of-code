module Data.AOC19.Day02 where

import           Control.Monad       (void)
import           Data.Digits         (digitsRev)
import           Data.Vector         (Vector, fromList, modify, slice, toList,
                                      (!))
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import           Text.Trifecta       (Parser, comma, integer, parseFromFile,
                                      sepBy)


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


program :: Parser (Vector Int)
program = fromList . map fromInteger <$> (integer `sepBy` comma)


runProgram :: Vector Int -> IO (Vector Int)
runProgram = runProgram' 0


runProgram' :: Int -> Vector Int -> IO (Vector Int)
runProgram' n state = step (digitsRev 10 instruction)
  where
    instruction :: Int
    instruction = state ! n

    step :: [Int] -> IO (Vector Int)
    step [1]       = step [1,0,0,0]
    step [1,1]     = step [1,1,0,0]
    step [1,0,1]   = step [1,0,1,0]
    step [1,0,c,b] =
      do let params = zipWith ($) (mkValue <$> [c,b,0]) $
                      toList (slice (n+1) 3 state)
         runProgram' (n+4) =<< runInstruction ADD params state
    step [2]       = step [2,0,0,0]
    step [2,1]     = step [2,1,0,0]
    step [2,0,1]   = step [2,0,1,0]
    step [2,0,c,b] =
      let params = zipWith ($) (mkValue <$> [c,b,0]) $
                   toList (slice (n+1) 3 state) in
        runProgram' (n+4) =<< runInstruction MUL params state
    step [3] =
      do vx <- ImmediateMode . read <$> getLine
         let params = [vx, PositionMode (state ! (n+1))]
         runProgram' (n+2) =<< runInstruction SET params state
    step [4]       = step [4,0,0]
    step [4,0,c]
      | c == 0 = go PositionMode
      | c == 1 = go ImmediateMode
      where
        go mode = runInstruction PRN [mode (state ! (n+1))] state >>=
                  runProgram' (n+2)
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
runInstruction SET [vx, PositionMode dst] state =
  do x <- handleValue vx state
     pure $ modify (\v -> MV.write v dst x) state
runInstruction PRN [vx] state =
  do print =<< handleValue vx state
     pure state
runInstruction _ _ _ = fail "Invalid instruction"


handleValue :: Value -> Vector Int -> IO Int
handleValue (PositionMode i)  = flip V.indexM i
handleValue (ImmediateMode n) = const (pure n)


partOne :: IO ()
partOne =
  do maybeProg <- parseFromFile program "../../../input/day05.txt"
     case maybeProg of
       Just prog -> void $ runProgram prog
       Nothing   -> error "No parse"
