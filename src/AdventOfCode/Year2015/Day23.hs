{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : AdventOfCode.Year2015.Day23
-- Description : Advent of Code 2015 Day 23: Opening the Turing Lock
-- Copyright   : (c) Eric Bailey, 2024
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
-- https://adventofcode.com/2015/day/23
module AdventOfCode.Year2015.Day23
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Lens (makeLenses, modifying, uses, view, (+=), (.~))
import Control.Monad (when)
import Control.Monad.State (State, execState)
import Data.Bool (bool)
import Data.Default (Default (def))
import Data.Function ((&))
import Data.Function.Pointless ((.:))
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

-- ------------------------------------------------------------------- [ Types ]

data Instruction
  = InstructionRegister !Operation !Register
  | InstructionOffset !Operation !Offset
  | InstructionRegisterOffset !Operation !Register !Offset
  deriving (Eq)

instance Show Instruction where
  show (InstructionRegister op r) = show op <> " " <> show r
  show (InstructionOffset op o) = show op <> " " <> show o
  show (InstructionRegisterOffset op r o) = show op <> " " <> show r <> ", " <> show o

data Operation
  = HLF
  | TPL
  | INC
  | JMP
  | JIE
  | JIO
  deriving (Eq, Show)

data Register
  = A
  | B
  deriving (Eq, Show)

type Offset = Int

data ComputerState = ComputerState
  { _cursor :: !Int,
    _registerA :: !Int,
    _registerB :: !Int
  }
  deriving (Eq, Generic, Default, Show)

makeLenses ''ComputerState

type Program = State ComputerState

-- ------------------------------------------------------------------ [ Puzzle ]

main :: IO ()
main = $(defaultMain)

partOne :: Vector Instruction -> Int
partOne = programExec def

partTwo :: Vector Instruction -> Int
partTwo = programExec $ def & (registerA .~ 1)

getInput :: IO (Vector Instruction)
getInput = parseInput (Vector.fromList <$> some instruction) $(inputFilePath)

-- ---------------------------------------------------------------- [ Programs ]

program :: Vector Instruction -> Program ()
program prog
  | Vector.null prog = pure ()
  | otherwise =
      ensuring (uses cursor (inRange (0, Vector.length prog - 1))) $
        do
          runInstruction =<< uses cursor (prog !)
          program prog

execProgram :: Vector Instruction -> ComputerState -> Int
execProgram = view registerB .: execState . program

programExec :: ComputerState -> Vector Instruction -> Int
programExec = flip execProgram

-- ------------------------------------------------------------ [ Instructions ]

runInstruction :: Instruction -> Program ()
runInstruction (InstructionRegister HLF r) =
  modifyingRegister r (`div` 2) *> moveCursor 1
runInstruction (InstructionRegister TPL r) =
  modifyingRegister r (* 3) *> moveCursor 1
runInstruction (InstructionRegister INC r) =
  modifyingRegister r (+ 1) *> moveCursor 1
runInstruction (InstructionOffset JMP o) = moveCursor o
runInstruction (InstructionRegisterOffset JIE r o) = jumpIf even r o
runInstruction (InstructionRegisterOffset JIO r o) = jumpIf (== 1) r o
runInstruction _ = error "Invalid instruction!"

moveCursor :: Offset -> Program ()
moveCursor o = cursor += o

jumpIf :: (Int -> Bool) -> Register -> Offset -> Program ()
jumpIf p r o = moveCursor . bool 1 o =<< usesRegister r p

-- ----------------------------------------------------------------- [ Parsers ]

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

register :: Parser Register
register =
  highlight Identifier $
    (A <$ symbolic 'a' <?> "register a")
      <|> (B <$ symbolic 'b' <?> "register b")

offset :: Parser Offset
offset = highlight Number $ fromInteger <$> integer <?> "offset"

-- ----------------------------------------------------------------- [ Helpers ]

ensuring :: (Monad m) => m Bool -> m () -> m ()
ensuring p s = p >>= flip when s

mkOp :: a -> String -> String -> Parser a
mkOp op repr desc = highlight Operator $ op <$ symbol repr <?> desc

modifyingRegister :: Register -> (Int -> Int) -> Program ()
modifyingRegister A = modifying registerA
modifyingRegister B = modifying registerB

usesRegister :: Register -> (Int -> Bool) -> Program Bool
usesRegister A = uses registerA
usesRegister B = uses registerB

-- --------------------------------------------------------------------- [ EOF ]
