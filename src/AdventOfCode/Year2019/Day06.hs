module AdventOfCode.Year2019.Day06 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Monad (void)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Text.Trifecta
  ( Parser,
    alphaNum,
    eof,
    manyTill,
    some,
    symbol,
    token,
  )

data Orbit
  = Orbit String String
  deriving (Eq)

instance Show Orbit where
  show (Orbit outer inner) = outer ++ ")" ++ inner

orbits :: Parser [Orbit]
orbits = manyTill (token orbit) eof

orbit :: Parser Orbit
orbit =
  do
    inner <- some alphaNum
    void (symbol ")")
    outer <- some alphaNum
    pure $ Orbit outer inner

directOrbits :: [Orbit] -> Map String String
directOrbits = foldr go Map.empty
  where
    go (Orbit outer inner) = Map.insert outer inner

indirectOrbits :: Map String String -> Map String [String]
indirectOrbits dorbs = Map.foldrWithKey go Map.empty dorbs
  where
    go :: String -> String -> Map String [String] -> Map String [String]
    go outer inner = Map.insert outer (go' [] (Map.lookup inner dorbs))
    go' inners Nothing = inners
    go' inners (Just inner') = go' (inner' : inners) (Map.lookup inner' dorbs)

minimumOrbitalTransfers :: String -> String -> Map String [String] -> Int
minimumOrbitalTransfers from to iorbs =
  let froms = reverse (iorbs ! from)
      tos = reverse (iorbs ! to)
   in 2
        + length (takeWhile (not . flip elem tos) froms)
        + length (takeWhile (not . flip elem froms) tos)

partOne :: [Orbit] -> Int
partOne orbs = Map.size dorbs + sum (fmap length iorbs)
  where
    dorbs = directOrbits orbs
    iorbs = indirectOrbits dorbs

partTwo :: [Orbit] -> Int
partTwo orbs = minimumOrbitalTransfers "YOU" "SAN" iorbs
  where
    dorbs = directOrbits orbs
    iorbs = indirectOrbits dorbs

main :: IO ()
main =
  do
    putStrLn "[2019] Day 6: Universal Orbit Map"
    input <- parseInput orbits $(inputFilePath)
    putStr "Part One: "
    print (partOne input)
    putStr "Part Two: "
    print (partTwo input)
