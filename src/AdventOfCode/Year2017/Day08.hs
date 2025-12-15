{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2017.Day08 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.Foldable (maximum)
import Data.Map.Strict qualified as Map
import Relude
import Text.Show qualified
import Text.Trifecta (Parser, choice, integer, letter, symbol, whiteSpace)

data Modification
  = Modify !BinOp !String !Int
  deriving (Eq)

instance Show Modification where
  show (Modify op reg val) =
    reg <> " " <> show op <> " " <> show val

evalModification :: (MonadState (Map String Int) m) => Modification -> m ()
evalModification (Modify op regAlter delta) =
  modify (Map.alter (Just . flip (evalBinOp op) delta . fromMaybe 0) regAlter)

data BinOp = Dec | Inc
  deriving (Eq)

instance Show BinOp where
  show = \case
    Dec -> "dec"
    Inc -> "inc"

data CondOp
  = CLT
  | CLE
  | CEQ
  | CNE
  | CGE
  | CGT
  deriving (Eq)

instance Show CondOp where
  show = \case
    CLT -> "<"
    CLE -> "<="
    CEQ -> "=="
    CNE -> "/="
    CGE -> ">="
    CGT -> ">"

data Conditional
  = Cond !CondOp !String !Int
  deriving (Eq)

instance Show Conditional where
  show (Cond cmp reg val) = reg <> " " <> show cmp <> " " <> show val

evalConditional :: (MonadState (Map String Int) m) => Conditional -> m Bool
evalConditional (Cond cmp reg n) =
  flip (condOp cmp) n <$> gets (fromMaybe 0 . Map.lookup reg)

data Instruction
  = Ins !Modification !Conditional
  deriving (Eq)

instance Show Instruction where
  show (Ins change cond) = show change <> " if " <> show cond

evalInstruction :: (MonadState (Map String Int) m) => Instruction -> m ()
evalInstruction (Ins change cond) =
  evalConditional cond
    >>= flip when (evalModification change)

main :: IO ()
main = $(defaultMainPuzzle)

emptyPuzzleState :: Map String Int
emptyPuzzleState = Map.empty

partOne :: Puzzle (NonEmpty Instruction) (Map String Int) Int
partOne = do
  ask >>= mapM_ evalInstruction
  gets maximum

partTwo :: Puzzle (NonEmpty Instruction) (Map String Int) Int
partTwo =
  maximum
    . fmap maximum
    . init
    <$> asks (foldl' go (Map.empty :| []))
  where
    go (mem :| history) ins =
      execState (evalInstruction ins) mem :| mem : history

getInput :: IO (NonEmpty Instruction)
getInput = parseInputAoC 2017 8 (NE.some instruction)

condOp :: CondOp -> (Int -> Int -> Bool)
condOp = \case
  CLT -> (<)
  CLE -> (<=)
  CEQ -> (==)
  CNE -> (/=)
  CGE -> (>=)
  CGT -> (>)

binOp :: Parser BinOp
binOp =
  (Dec <$ symbol "dec")
    <|> (Inc <$ symbol "inc")

evalBinOp :: BinOp -> Int -> Int -> Int
evalBinOp = \case
  Dec -> (-)
  Inc -> (+)

modification :: Parser Modification
modification =
  flip Modify
    <$> (some letter <* whiteSpace)
    <*> binOp
    <*> (fromInteger <$> integer)

compareReg :: Map String Int -> CondOp -> String -> Int -> Bool
compareReg mem cmp = condOp cmp . fromMaybe 0 . flip Map.lookup mem

conditional :: Parser Conditional
conditional =
  flip Cond
    <$> (symbol "if" *> some letter <* whiteSpace)
    <*> comparison
    <*> (fromInteger <$> integer)

instruction :: Parser Instruction
instruction = Ins <$> modification <*> conditional

comparison :: Parser CondOp
comparison =
  choice
    [ CLE <$ symbol "<=",
      CLT <$ symbol "<",
      CEQ <$ symbol "==",
      CNE <$ symbol "!=",
      CGE <$ symbol ">=",
      CGT <$ symbol ">"
    ]

getExample :: IO (NonEmpty Instruction)
getExample = parseString (NE.some instruction) example

example :: String
example =
  "b inc 5 if a > 1\n\
  \a inc 1 if b < 5\n\
  \c dec -10 if a >= 1\n\
  \c inc -20 if c == 10\n"
