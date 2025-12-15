{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2020.Day19
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (count)
import Data.Functor.Foldable (hylo)
import Data.IntMap ((!))
import Data.IntMap qualified as IntMap
import Relude
import Text.Trifecta
  ( Parser,
    lower,
    natural,
    newline,
    notFollowedBy,
    sepBy,
    sepEndBy,
    surroundedBy,
    symbol,
    try,
  )

data Rule a
  = SimpleRule Char
  | CompoundRule [[a]]
  deriving (Eq, Show, Functor)

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO (IntMap (Rule Int), [String])
getInput = parseInputAoC 2020 19 puzzleInput
  where
    puzzleInput = (,) <$> rules <*> messages
    rules = foldr (uncurry IntMap.insert) IntMap.empty <$> some (simpleRule <|> compoundRule)
    messages = some lower `sepEndBy` newline

partOne :: SimplePuzzle (IntMap (Rule Int), [String]) Int
partOne = asks solve

partTwo :: SimplePuzzle (IntMap (Rule Int), [String]) Int
partTwo = asks (solve . first patchRules)
  where
    patchRules =
      mappend
        $ IntMap.fromList
          [ (8, CompoundRule [[42], [42, 8]]),
            (11, CompoundRule [[42, 31], [42, 11, 31]])
          ]

solve :: (IntMap (Rule Int), [String]) -> Int
solve = uncurry $ \rulesMap -> count (any null . match rulesMap)

match :: IntMap (Rule Int) -> String -> [String]
match rulesMap = hylo matchRuleAlg (rulesMap !) 0

matchRuleAlg :: Rule (String -> [String]) -> String -> [String]
matchRuleAlg (SimpleRule c') (c : cs) = cs <$ guard (c == c')
matchRuleAlg (SimpleRule _) [] = []
matchRuleAlg (CompoundRule ruleses) str =
  concatMap (flip (foldr (>=>) pure) str) ruleses

rule :: Parser (Rule Int) -> Parser (Int, Rule Int)
rule = try . (((,) <$> posInt <* symbol ":") <*>)

simpleRule :: Parser (Int, Rule Int)
simpleRule = rule (SimpleRule <$> lower `surroundedBy` symbol "\"")

compoundRule :: Parser (Int, Rule Int)
compoundRule = rule (CompoundRule <$> (many posInt' `sepBy` symbol "|"))

posInt' :: Parser Int
posInt' = try (posInt <* notFollowedBy (symbol ":"))

posInt :: Parser Int
posInt = fromInteger <$> natural
