module AdventOfCode.Year2015.Day19 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative (many, some, (<|>))
import Control.Arrow (second)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.Trifecta (Parser, lower, string, symbol, token, try, upper)

data Problem a = (Grammar a) :-> [a]
  deriving (Show)

type Grammar a = Map a [[a]]

type Molecule = [Atom]

type Atom = String

main :: IO ()
main = $(defaultMain)

getInput :: IO (Problem Atom)
getInput = parseInput ((:->) <$> replacements <*> molecule) $(inputFilePath)

partOne :: Problem Atom -> Int
partOne = length . nub . expand

partTwo :: Problem Atom -> Int
partTwo (_ :-> goal) =
  foldl1 (-) $
    [length, count "Ar", count "Rn", (2 *) . count "Y", const 1] <*> [goal]

expand :: Problem Atom -> [Molecule]
expand (formulae :-> goal) = expansions formulae goal

-- https://github.com/amalloy/advent-of-code/blob/master/day19/src.hs
expansions :: (Ord a) => Grammar a -> [a] -> [[a]]
expansions _ [] = []
expansions m (x : xs) = leaveAlone ++ expandHere
  where
    leaveAlone = (x :) <$> expansions m xs
    expandHere = M.findWithDefault [] x m <&> (++ xs)

molecule :: Parser Molecule
molecule = some atom

atom :: Parser Atom
atom = try ((:) <$> upper <*> many lower) <|> string "e"

replacements :: Parser (Map Atom [[Atom]])
replacements =
  M.fromListWith (++) . map (second pure) <$> some (try replacement)

replacement :: Parser (Atom, Molecule)
replacement = (,) <$> token atom <* symbol "=>" <*> token molecule

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)
