{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2019.Day06 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (iterateMaybe)
import Data.Map ((!), (!?))
import Data.Map qualified as Map
import Relude
import Text.Show qualified
import Text.Trifecta (Parser, alphaNum, eof, manyTill, symbol, token)

data Orbit
  = Orbit !String !String
  deriving (Eq, Generic)

instance Show Orbit where
  show (Orbit outer inner) = outer ++ ")" ++ inner

orbits :: Parser [Orbit]
orbits = manyTill (token orbit) eof

orbit :: Parser Orbit
orbit =
  flip Orbit
    <$> (some alphaNum <* symbol ")")
    <*> some alphaNum

directOrbits :: [Orbit] -> Map String String
directOrbits = foldr go Map.empty
  where
    go (Orbit outer inner) = Map.insert outer inner

indirectOrbits :: Map String String -> Map String [String]
indirectOrbits dorbs = Map.foldrWithKey outer Map.empty dorbs
  where
    outer outerOrbit innerOrbit =
      Map.insert outerOrbit
        $ maybe [] (iterateMaybe (dorbs !?))
        $ Map.lookup innerOrbit dorbs

minimumOrbitalTransfers :: String -> String -> Map String [String] -> Int
minimumOrbitalTransfers from to iorbs = 2 + go froms tos + go tos froms
  where
    go these those = length (takeWhile (not . flip elem those) these)
    froms = iorbs ! from
    tos = iorbs ! to

partOne :: SimplePuzzle [Orbit] Int
partOne =
  asks
    $ directOrbits
    >>> (Map.size &&& sum . fmap length . indirectOrbits)
    >>> uncurry (+)

partTwo :: SimplePuzzle [Orbit] Int
partTwo =
  asks
    $ directOrbits
    >>> indirectOrbits
    >>> minimumOrbitalTransfers "YOU" "SAN"

getInput :: IO [Orbit]
getInput = parseInputAoC 2019 6 orbits

main :: IO ()
main = $(defaultMainPuzzle)
