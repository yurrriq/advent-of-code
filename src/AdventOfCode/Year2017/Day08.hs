module AdventOfCode.Year2017.Day08 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Trifecta (Parser, choice, integer, letter, some, symbol, whiteSpace)

data Op = Dec | Inc
  deriving (Eq, Show)

data Comparison
  = CLT
  | CLE
  | CEQ
  | CNE
  | CGE
  | CGT

data Instruction = Ins Op Text Int Comparison Text Int

main :: IO ()
main = $(defaultMain)

partOne :: [Instruction] -> Int
partOne = maximum . Map.elems . foldl' go Map.empty
  where
    go mem (Ins op regAlter delta cmp regCompare comparate) =
      if compareReg mem cmp regCompare comparate
        then Map.alter (Just . flip (binOp op) delta . fromMaybe 0) regAlter mem
        else mem

partTwo :: [Instruction] -> Int
partTwo = maximum . map (maximum . Map.elems) . init . foldl' go [Map.empty]
  where
    go (mem : history) (Ins op regAlter delta cmp regCompare comparate) =
      mem' : mem : history
      where
        mem' =
          if compareReg mem cmp regCompare comparate
            then Map.alter (Just . flip (binOp op) delta . fromMaybe 0) regAlter mem
            else mem
    go [] _ = []

getInput :: IO [Instruction]
getInput = parseInput (some instruction) $(inputFilePath)

kompare :: Comparison -> (Int -> Int -> Bool)
kompare cmp = case cmp of
  CLT -> (<)
  CLE -> (<=)
  CEQ -> (==)
  CNE -> (/=)
  CGE -> (>=)
  CGT -> (>)

binOp :: Op -> Int -> Int -> Int
binOp Dec = (-)
binOp Inc = (+)

compareReg :: Map Text Int -> Comparison -> Text -> Int -> Bool
compareReg mem cmp = kompare cmp . fromMaybe 0 . flip Map.lookup mem

instruction :: Parser Instruction
instruction =
  do
    regAlter <- Text.pack <$> some letter <* whiteSpace
    op <- (Dec <$ symbol "dec") <|> (Inc <$ symbol "inc")
    delta <- fromInteger <$> integer
    regCompare <- symbol "if" *> (Text.pack <$> some letter) <* whiteSpace
    cmp <- comparison
    comparate <- fromInteger <$> integer
    pure (Ins op regAlter delta cmp regCompare comparate)

comparison :: Parser Comparison
comparison =
  choice
    [ CLE <$ symbol "<=",
      CLT <$ symbol "<",
      CEQ <$ symbol "==",
      CNE <$ symbol "!=",
      CGE <$ symbol ">=",
      CGT <$ symbol ">"
    ]
