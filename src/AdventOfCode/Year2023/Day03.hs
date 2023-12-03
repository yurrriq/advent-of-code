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

instance Show Schematic where
  show (Schematic hm) =
    intercalate "\n" $
      [ concat
          [ maybe "" show (HM.lookup (V2 x y) hm)
            | x <- [0 .. width]
          ]
        | y <- [0 .. height]
      ]
    where
      V2 width height = maximum (HM.keys hm)

main :: IO ()
main = $(defaultMain)

partOne :: Schematic -> Int
partOne (Schematic hm) = let Number n = mconcat ds in n
  where
    ds = HM.elems (HM.filterWithKey adjacentToSymbol hm)
    adjacentToSymbol (V2 x y) (Number n) =
      any isSymbol $
        foldr ((<>) . neighborsOf) Set.empty $
          flip V2 y <$> [x .. x + length (show n) - 1]
    adjacentToSymbol _ _ = False
    isSymbol k = case HM.lookup k hm of
      Just (Symbol _) -> True
      _ -> False

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
    go y x (hm', []) d = (HM.insert (V2 x y) d hm', [])
    go y x (hm', ns) d =
      ( HM.insert (V2 (x - length ns) y) (Number (undigits' ns)) $
          HM.insert (V2 x y) d hm',
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
