{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AdventOfCode.Year2023.Day03 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMain, inputFilePath)
import AdventOfCode.Util (neighborsOf)
import Control.Applicative ((<|>))
import Control.Lens (ifoldl')
import Data.Char (digitToInt)
import Data.FastDigits (undigits)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Linear (V2 (..))
import Text.Trifecta hiding (parseString)

data Datum
  = Number Int
  | Symbol Char
  | Period
  deriving (Eq)

instance Show Datum where
  show (Number n) = show n
  show (Symbol s) = [s]
  show Period = "."

instance Semigroup Datum where
  Number x <> Number y = Number (x + y)
  _ <> _ = undefined

instance Monoid Datum where
  mempty = Number 0

instance Ord Datum where
  Number x <= Number y = x <= y
  _ <= _ = undefined

newtype Schematic = Schematic {unSchematic :: HashMap (V2 Int) Datum}

main :: IO ()
main = $(defaultMain)

partOne :: Schematic -> Int
partOne (Schematic hm) = let Number n = mconcat ds in n
  where
    ds = HM.elems (HM.filterWithKey (isNumberAdjacentToSymbol hm) hm)

isNumberAdjacentToSymbol :: HashMap (V2 Int) Datum -> V2 Int -> Datum -> Bool
isNumberAdjacentToSymbol hm (V2 x y) (Number n) =
  any isSymbol $ allNeighborsOf (V2 x y) n
  where
    isSymbol k = case HM.lookup k hm of
      Just (Symbol _) -> True
      _ -> False
isNumberAdjacentToSymbol _ _ _ = False

allNeighborsOf :: (Show a) => V2 Int -> a -> Set (V2 Int)
allNeighborsOf (V2 x y) n =
  flip Set.difference (Set.fromList points) $
    foldr ((<>) . neighborsOf) Set.empty points
  where
    points = flip V2 y <$> [x .. x + length (show n) - 1]

partOneExample :: IO Int
partOneExample = partOne <$> parseString schematic example

partTwo :: Schematic -> Int
partTwo = undefined

getInput :: IO Schematic
getInput = parseInput schematic $(inputFilePath)

schematic :: Parser Schematic
schematic = mkSchematic <$> (some datum `sepEndBy` newline)

mkSchematic :: [[Datum]] -> Schematic
mkSchematic xxs = Schematic hm
  where
    (hm, _) = ifoldl' (ifoldl' . go) (HM.empty, []) xxs
    go _ _ (hm', ns) (Number n) = (hm', n : ns)
    go y x (hm', []) d@(Symbol _) = (HM.insert (V2 x y) d hm', [])
    go _ _ (hm', []) Period = (hm', [])
    go y x (hm', ns) d@(Symbol _) =
      ( HM.insert (V2 (x - length ns) y) (Number (undigits' ns)) $
          HM.insert (V2 x y) d hm',
        []
      )
    go y x (hm', ns) Period =
      ( HM.insert (V2 (x - length ns) y) (Number (undigits' ns)) hm',
        []
      )
    undigits' = fromInteger . undigits (10 :: Int)

datum :: Parser Datum
datum =
  Period <$ char '.'
    <|> Number . digitToInt <$> digit
    <|> Symbol <$> notChar '\n'

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
