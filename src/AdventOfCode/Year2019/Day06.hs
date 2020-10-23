module AdventOfCode.Year2019.Day06 where

import AdventOfCode.Util (parseInput)
import Control.Monad (void)
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HM
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

directOrbits :: [Orbit] -> HM.HashMap String String
directOrbits = foldr go HM.empty
  where
    go :: Orbit -> HM.HashMap String String -> HM.HashMap String String
    go (Orbit outer inner) = HM.insert outer inner

indirectOrbits :: HM.HashMap String String -> HM.HashMap String [String]
indirectOrbits dorbs = HM.foldrWithKey go HM.empty dorbs
  where
    go :: String -> String -> HM.HashMap String [String] -> HM.HashMap String [String]
    go outer inner = HM.insert outer (go' [] (HM.lookup inner dorbs))
    go' inners Nothing = inners
    go' inners (Just inner') = go' (inner' : inners) (HM.lookup inner' dorbs)

minimumOrbitalTransfers :: String -> String -> HM.HashMap String [String] -> Int
minimumOrbitalTransfers from to iorbs =
  let froms = reverse (iorbs ! from)
      tos = reverse (iorbs ! to)
   in 2
        + (length (takeWhile (not . flip elem tos) froms))
        + (length (takeWhile (not . flip elem froms) tos))

partOne :: IO Int
partOne =
  do
    orbs <- parseInput orbits "input/2019/day06.txt"
    let dorbs = directOrbits orbs
    let iorbs = indirectOrbits dorbs
    pure $ HM.size dorbs + sum (HM.map length iorbs)

partTwo :: IO Int
partTwo =
  do
    orbs <- parseInput orbits "input/2019/day06.txt"
    let dorbs = directOrbits orbs
    let iorbs = indirectOrbits dorbs
    pure $ minimumOrbitalTransfers "YOU" "SAN" iorbs

main :: IO ()
main =
  do
    putStrLn "[2019] Day 6: Universal Orbit Map"
    putStr "Part One: "
    print =<< partOne
    putStr "Part Two: "
    print =<< partTwo
