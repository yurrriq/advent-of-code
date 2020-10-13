module Data.AOC19.Day06 where

import Control.Monad (void)
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HM
import Text.Trifecta
  ( Parser,
    alphaNum,
    eof,
    manyTill,
    parseFromFile,
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

partOne :: IO ()
partOne =
  do
    res <- parseFromFile orbits "../../../input/day06.txt"
    case res of
      Nothing -> error "No parse"
      Just orbs ->
        do
          let dorbs = directOrbits orbs
          let iorbs = indirectOrbits dorbs
          print $ HM.size dorbs + sum (HM.map length iorbs)

partTwo :: IO ()
partTwo =
  do
    res <- parseFromFile orbits "../../../input/day06.txt"
    case res of
      Nothing -> error "No parse"
      Just orbs ->
        do
          let dorbs = directOrbits orbs
          let iorbs = indirectOrbits dorbs
          print $ minimumOrbitalTransfers "YOU" "SAN" iorbs
