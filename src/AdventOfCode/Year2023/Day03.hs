{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2023.Day03 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (neighborsOf)
import Control.Lens (ifoldl')
import Data.Char (digitToInt)
import Data.FastDigits (undigits)
import Data.HashMap.Strict qualified as HM
import Data.Set qualified as Set
import Linear (V2 (..))
import Relude
import Text.Show qualified
import Text.Trifecta hiding (parseString)

data Datum
  = Number !Int
  | Symbol !Char
  | Period
  deriving (Eq)

instance Show Datum where
  show (Number n) = show n
  show (Symbol s) = [s]
  show Period = "."

instance Semigroup Datum where
  Number x <> Number y = Number (x + y)
  _ <> _ = error "Semigroup is only defined for Number."

instance Monoid Datum where
  mempty = Number 0

instance Ord Datum where
  Number x <= Number y = x <= y
  _ <= _ = error "Ord is only defined for Number."

withNumber :: (MonadFail m) => (Int -> a) -> Datum -> m a
withNumber f = \case
  Number n -> pure (f n)
  _notNumber -> fail "Not a number"

type Schematic = HashMap (V2 Int) Datum

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle Schematic Int
partOne = do
  hm <- ask
  HM.filterWithKey (isNumberAdjacentToSymbol hm) hm
    & HM.elems
    & mconcat
    & withNumber id

isNumberAdjacentToSymbol :: HashMap (V2 Int) Datum -> V2 Int -> Datum -> Bool
isNumberAdjacentToSymbol hm (V2 x y) (Number n) =
  any isSymbol $ allNeighborsOf (V2 x y) n
  where
    isSymbol k = case HM.lookup k hm of
      Just (Symbol _) -> True
      _notNumber -> False
isNumberAdjacentToSymbol _ _ _ = False

allNeighborsOf :: (Show a) => V2 Int -> a -> Set (V2 Int)
allNeighborsOf (V2 x y) n =
  flip Set.difference (Set.fromList points)
    $ foldr ((<>) . neighborsOf) Set.empty points
  where
    points = flip V2 y <$> [x .. x + length (show @String n) - 1]

partOneExample :: IO Int
partOneExample = evaluatingPuzzle partOne =<< getExample

partTwo :: SimplePuzzle Schematic Int
partTwo = fail "Not yet implemented"

getInput :: IO Schematic
getInput = parseInputAoC 2023 3 schematic

schematic :: Parser Schematic
schematic = mkSchematic <$> (some datum `sepEndBy` newline)

mkSchematic :: [[Datum]] -> Schematic
mkSchematic xxs = hm
  where
    (hm, _) = ifoldl' (ifoldl' . go) (HM.empty, []) xxs
    go _ _ (hm', ns) (Number n) = (hm', n : ns)
    go y x (hm', []) d@(Symbol _) = (HM.insert (V2 x y) d hm', [])
    go _ _ (hm', []) Period = (hm', [])
    go y x (hm', ns) d@(Symbol _) =
      ( HM.insert (V2 (x - length ns) y) (Number (undigits' ns))
          $ HM.insert (V2 x y) d hm',
        []
      )
    go y x (hm', ns) Period =
      ( HM.insert (V2 (x - length ns) y) (Number (undigits' ns)) hm',
        []
      )
    undigits' = fromInteger . undigits (10 :: Int)

datum :: Parser Datum
datum =
  Period
    <$ char '.'
    <|> Number
    . digitToInt
    <$> digit
    <|> Symbol
    <$> notChar '\n'

getExample :: IO Schematic
getExample = parseString schematic example

example :: String
example =
  intercalate
    "\n"
    [ "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    ]
