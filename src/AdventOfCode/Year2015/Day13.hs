{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2015.Day13 where

import AdventOfCode.Input
import AdventOfCode.TH
import Control.Applicative ((<|>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (permutations)
import Data.Tuple (swap)
import Text.Parser.Token.Highlight
import Text.Trifecta

type PotentialHappiness = HashMap String (HashMap String Integer)

main :: IO ()
main = $(defaultMain)

getInput :: IO PotentialHappiness
getInput =
  HM.fromListWith HM.union
    <$> parseInput (potentialHappiness `sepEndBy` newline) $(inputFilePath)

partOne :: PotentialHappiness -> Integer
partOne = maxHappiness

partTwo :: PotentialHappiness -> Integer
partTwo input =
  maxHappiness . HM.union (HM.map (HM.insert "Me" 0) input) $
    HM.singleton "Me" . HM.fromList $
      map (,0) (invitees input)

maxHappiness :: PotentialHappiness -> Integer
maxHappiness input =
  maximum
    [ sum [(input HM.! person) HM.! neighbor | (person, neighbor) <- xs]
      | xs <- map pairs (permutations (invitees input))
    ]

potentialHappiness :: Parser (String, HashMap String Integer)
potentialHappiness =
  do
    person <- name
    n <- wouldGain <|> (negate <$> wouldLose)
    neighbor <- bySittingNextTo name
    pure (person, HM.singleton neighbor n)

name :: Parser String
name = highlight Identifier (token (some letter))

wouldGain :: Parser Integer
wouldGain = highlight Operator (symbol "would gain") *> natural

wouldLose :: Parser Integer
wouldLose = highlight Operator (symbol "would lose") *> natural

bySittingNextTo :: Parser a -> Parser a
bySittingNextTo = between (string "happiness units by sitting next to ") (char '.')

pairs :: [a] -> [(a, a)]
pairs xs = concatMap ((:) <$> swap <*> return) ps
  where
    ps = (head xs, last xs) : zip xs (tail xs)

invitees :: PotentialHappiness -> [String]
invitees = HM.keys
