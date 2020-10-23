module Data.AOC19.Day05

import Data.Fin
import Data.Vect
import Effects


total
slice : (i, n : Nat) -> Vect (i + (n + m)) a -> Vect n a
slice i n = take n . drop i


namespace IntCode
  data State : Type where
    Running : {n : Nat} -> (ptr : Fin n) -> (state : Vect n Int) -> State
    NotRunning : State

  data ParameterMode
    = PositionMode
    | ImmediateMode

  data OpCode
    = ADD
    | MUL
    | SET
    | PRN
    | END

  data Program : State -> Type where
    Halted : Program NotRunning
    MkP : (ptr : Fin n) -> (state : Vect n Int) -> Program (Running ptr state)

  data Instruction : Effect where
    Add : sig Instruction (Vect (S (S (S (S k)))) Int)
              (Program (Running ptr state))
              (\state' => Program (Running (shift 4 ptr) state'))

  Handler Instruction m where
    handle (MkP (ptr state {n=S (S (S (S j)))}) Add k =
      let dst = Vect.index (shift 3 ptr) state in
      ?rhs_1


-- program :: Parser (Vector Int)
-- program = fromList . map fromInteger <$> (natural `sepBy` comma)


-- runProgram :: Vector Int -> Vector Int
-- runProgram = go 0
--   where
--     go n state
--       | state ! n == 99 = state
--       | otherwise       = go (n + 4) $ step (toList (V.slice n 4 state))
--       where
--         step [1, x, y, dst] = modify (runOp (+) x y dst) state
--         step [2, x, y, dst] = modify (runOp (*) x y dst) state
--         step _              = state

--     runOp f x y dst v = write v dst =<< f <$> MV.read v x <*> MV.read v y


-- example1 :: Vector Int
-- example1 =
--     case parseString program mempty "1,9,10,3,2,3,11,0,99,30,40,50" of
--       Success prog   -> prog
--       Failure reason -> error (show reason)
