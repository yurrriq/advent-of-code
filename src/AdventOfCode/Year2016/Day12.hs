{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module AdventOfCode.Year2016.Day12 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Control.Lens
  ( element,
    makeLenses,
    over,
    set,
    use,
    uses,
    (%=),
    (+=),
    (^?),
    (^?!),
  )
import Control.Monad.State (State, evalState)
import Data.Bool (bool)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear.V4 (V4 (..))
import Text.Trifecta (Parser, char, choice, integer', some, symbol, token)

data Register = A | B | C | D
  deriving (Eq, Enum, Show)

data Instruction
  = CPY (Either Register Integer) Register
  | INC Register
  | DEC Register
  | JNZ (Either Register Integer) Integer
  deriving (Eq, Show)

data ProgState = ProgState
  { _cursor :: Int,
    _instructions :: Vector Instruction,
    _registers :: V4 Integer
  }
  deriving (Eq, Show)

makeLenses ''ProgState

main :: IO ()
main = $(defaultMain)

partOne :: Vector Instruction -> Integer
partOne = evalState assembunny . flip (ProgState 0) (pure 0)

partTwo :: Vector Instruction -> Integer
partTwo = undefined

getInput :: IO (Vector Instruction)
getInput = parseInput (V.fromList <$> some instruction) $(inputFilePath)

assembunny :: State ProgState Integer
assembunny =
  do
    i <- use cursor
    maybeInstruction <- uses instructions (^? element i)
    case maybeInstruction of
      Just ins -> runInstruction ins *> assembunny
      Nothing -> uses registers (readRegister A)

runInstruction :: Instruction -> State ProgState ()
runInstruction = \case
  CPY regOrIns reg ->
    do
      regs <- use registers
      registers %= set (element (fromEnum reg)) (handleRegisterOrInteger regs regOrIns)
      cursor += 1
  INC reg ->
    do
      registers %= over (element (fromEnum reg)) (+ 1)
      cursor += 1
  DEC reg ->
    do
      registers %= over (element (fromEnum reg)) (subtract 1)
      cursor += 1
  JNZ regOrIns delta ->
    do
      x <- uses registers (`handleRegisterOrInteger` regOrIns)
      cursor += bool 1 (fromInteger delta) (0 /= x)

handleRegisterOrInteger :: V4 Integer -> Either Register Integer -> Integer
handleRegisterOrInteger regs = either ((regs ^?!) . element . fromEnum) id

readRegister :: Register -> V4 a -> a
readRegister reg regs = regs ^?! element (fromEnum reg)

instruction :: Parser Instruction
instruction =
  choice
    [ CPY <$ symbol "cpy" <*> registerOrInteger' <*> register,
      INC <$ symbol "inc" <*> register,
      DEC <$ symbol "dec" <*> register,
      JNZ <$ symbol "jnz" <*> registerOrInteger' <*> token integer'
    ]

registerOrInteger' :: Parser (Either Register Integer)
registerOrInteger' = Left <$> register <|> Right <$> token integer'

register :: Parser Register
register = choice $ zipWith (<$) [A, B, C, D] (token . char <$> "abcd")

example :: IO (Vector Instruction)
example =
  parseString (V.fromList <$> some instruction) $
    unlines
      [ "cpy 41 a",
        "inc a",
        "inc a",
        "dec a",
        "jnz a 2",
        "dec a"
      ]
