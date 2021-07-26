module AdventOfCode.Year2020.Day07 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Applicative ((<|>))
import Data.Bool (bool)
import Data.Digraph (Digraph)
import qualified Data.Digraph as DG
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Monoid (Sum (..))
import qualified Data.Set as S
import Text.Trifecta

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne input
    putStr "Part Two: "
    print $ partTwo input

partOne :: Digraph String Int -> Int
partOne = (M.! "shiny gold") . fmap S.size . DG.allAnscestors

partTwo :: Digraph String Int -> Int
partTwo = (M.! "shiny gold") . fmap getSum . DG.foldMap (const 0) go
  where
    go n z = Sum n * (z + 1)

getInput :: IO (Digraph String Int)
getInput = parseInput (M.fromList <$> rule `sepEndBy` newline) $(inputFilePath)

rule :: Parser (String, Map String Int)
rule =
  do
    bag <- bagName <* string "bags contain" <* space
    contents <- M.fromList . catMaybes <$> commaSep nBags <* char '.'
    pure (bag, contents)

bagName :: Parser String
bagName = unwords <$> count 2 (word <* space)

nBags :: Parser (Maybe (String, Int))
nBags =
  try (Nothing <$ string "no other bags")
    <|> do
      n <- fromInteger <$> natural
      bag <- bagName <* bagOrBags n
      pure $ Just (bag, n)

bagOrBags :: Int -> Parser String
bagOrBags n = string (bool "bag" "bags" (n > 1))

word :: Parser String
word = some lower
