{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2015.Day13 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Applicative ((<|>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List (permutations)
import Data.List.Extra (sumOn')
import Data.Semigroup (Max (..), getMax)
import Text.Parser.Token.Highlight
import Text.Trifecta

type PotentialHappiness = HashMap String (HashMap String Int)

main :: IO ()
main = $(defaultMain)

getInput :: IO PotentialHappiness
getInput =
  HM.fromListWith HM.union
    <$> parseInput (potentialHappiness `sepEndBy` newline) $(inputFilePath)

partOne :: PotentialHappiness -> Int
partOne = maxHappiness

partTwo :: PotentialHappiness -> Int
partTwo input =
  partOne . HM.union (HM.map (HM.insert "Me" 0) input) $
    HM.singleton "Me" . HM.fromList $
      map (,0) (invitees input)

maxHappiness :: PotentialHappiness -> Int
maxHappiness input =
  getMax . foldMap (Max . sumOn' happiness . pairs) $
    permutations . invitees $
      input
  where
    happiness (person, neighbor) =
      input HM.! person HM.! neighbor
        + input HM.! neighbor HM.! person

potentialHappiness :: Parser (String, HashMap String Int)
potentialHappiness = do
  person <- name
  n <- wouldGain <|> (negate <$> wouldLose)
  neighbor <- bySittingNextTo name
  pure (person, HM.singleton neighbor (fromInteger n))

name :: Parser String
name = highlight Identifier (token (some letter))

wouldGain :: Parser Integer
wouldGain = highlight Operator (symbol "would gain") *> natural

wouldLose :: Parser Integer
wouldLose = highlight Operator (symbol "would lose") *> natural

bySittingNextTo :: Parser a -> Parser a
bySittingNextTo = between (string "happiness units by sitting next to ") (char '.')

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (drop 1 (cycle xs))

invitees :: PotentialHappiness -> [String]
invitees = HM.keys
