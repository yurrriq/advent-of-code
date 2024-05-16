{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module AdventOfCode.Year2015.Day23
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Lens (makeLenses, modifying, uses, view, (+=))
import Control.Monad (when)
import Control.Monad.State (State, execState)
import Data.Bool (bool)
import Data.Default (Default (..))
import Data.Ix (inRange)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Text.Parser.Token.Highlight (Highlight (..))
import Text.Trifecta
  ( Parser,
    comma,
    highlight,
    integer,
    some,
    symbol,
    symbolic,
    (<?>),
  )

data Register
  = A
  | B
  deriving (Eq, Show)

type Offset = Int

data Operation
  = HLF
  | TPL
  | INC
  | JMP
  | JIE
  | JIO
  deriving (Eq, Show)

data Instruction
  = InstructionRegister !Operation !Register
  | InstructionOffset !Operation !Offset
  | InstructionRegisterOffset !Operation !Register !Offset
  deriving (Eq)

instance Show Instruction where
  show (InstructionRegister op r) = show op <> " " <> show r
  show (InstructionOffset op o) = show op <> " " <> show o
  show (InstructionRegisterOffset op r o) = show op <> " " <> show r <> ", " <> show o

data ComputerState = ComputerState
  { _cursor :: !Int,
    _registerA :: !Int,
    _registerB :: !Int
  }
  deriving (Eq, Generic, Default, Show)

makeLenses ''ComputerState

main :: IO ()
main = $(defaultMain)

partOne :: Vector Instruction -> Int
partOne = view registerB . flip execState initialState . program

partTwo :: Vector Instruction -> Int
partTwo = undefined

getInput :: IO (Vector Instruction)
getInput = parseInput (Vector.fromList <$> some instruction) $(inputFilePath)

initialState :: ComputerState
initialState = def

program :: Vector Instruction -> State ComputerState ()
program prog
  | Vector.null prog = pure ()
  | otherwise =
      ensuring (uses cursor (inRange (0, Vector.length prog - 1))) $
        do
          runInstruction =<< uses cursor (prog !)
          program prog

ensuring :: (Monad m) => m Bool -> m () -> m ()
ensuring p s = p >>= flip when s

runInstruction :: Instruction -> State ComputerState ()
runInstruction (InstructionRegister HLF r) =
  modifyingRegister r (`div` 2) *> moveCursor 1
runInstruction (InstructionRegister TPL r) =
  modifyingRegister r (* 3) *> moveCursor 1
runInstruction (InstructionRegister INC r) =
  modifyingRegister r (+ 1) *> moveCursor 1
runInstruction (InstructionOffset JMP o) = moveCursor o
runInstruction (InstructionRegisterOffset JIE r o) =
  moveCursor . bool 1 (fromIntegral o) =<< usesRegister r even
runInstruction (InstructionRegisterOffset JIO r o) =
  moveCursor . bool 1 (fromIntegral o) =<< usesRegister r (== 1)
runInstruction _ = error "Invalid instruction!"

usesRegister :: Register -> (Int -> Bool) -> State ComputerState Bool
usesRegister A = uses registerA
usesRegister B = uses registerB

modifyingRegister :: Register -> (Int -> Int) -> State ComputerState ()
modifyingRegister A = modifying registerA
modifyingRegister B = modifying registerB

moveCursor :: Int -> State ComputerState ()
moveCursor o = cursor += o

instruction :: Parser Instruction
instruction =
  highlight Statement $
    InstructionRegister
      <$> (half <|> triple <|> increment)
      <*> register
      <|> InstructionOffset
        <$> jump
        <*> offset
      <|> InstructionRegisterOffset
        <$> (jumpIfEven <|> jumpIfOne)
        <*> register
        <* comma
        <*> offset

half, triple, increment, jump, jumpIfEven, jumpIfOne :: Parser Operation
half = mkOp HLF "hlf" "half"
triple = mkOp TPL "tpl" "triple"
increment = mkOp INC "inc" "increment"
jump = mkOp JMP "jmp" "jump"
jumpIfEven = mkOp JIE "jie" "jump if even"
jumpIfOne = mkOp JIO "jio" "jump if one"

mkOp :: a -> String -> String -> Parser a
mkOp op repr desc = highlight Operator $ op <$ symbol repr <?> desc

register :: Parser Register
register =
  highlight Identifier $
    (A <$ symbolic 'a' <?> "register a")
      <|> (B <$ symbolic 'b' <?> "register b")

offset :: Parser Offset
offset = highlight Number $ fromInteger <$> integer <?> "offset"
