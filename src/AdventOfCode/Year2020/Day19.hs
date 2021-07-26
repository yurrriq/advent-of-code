{-# LANGUAGE DeriveFunctor #-}

module AdventOfCode.Year2020.Day19
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (count)
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Monad (guard, (>=>))
import Data.Functor.Foldable (hylo)
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap
import Text.Trifecta
  ( Parser,
    lower,
    many,
    natural,
    newline,
    notFollowedBy,
    sepBy,
    sepEndBy,
    some,
    surroundedBy,
    symbol,
    try,
  )

data Rule a
  = SimpleRule Char
  | CompoundRule [[a]]
  deriving (Eq, Show, Functor)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne input
    putStr "Part Two: "
    print $ partTwo input

getInput :: IO (IntMap (Rule Int), [String])
getInput = parseInput puzzleInput $(inputFilePath)
  where
    puzzleInput = (,) <$> rules <*> messages
    rules = foldr (uncurry IntMap.insert) IntMap.empty <$> some (simpleRule <|> compoundRule)
    messages = some lower `sepEndBy` newline

partOne :: (IntMap (Rule Int), [String]) -> Int
partOne = uncurry solve

partTwo :: (IntMap (Rule Int), [String]) -> Int
partTwo = uncurry solve . first patchRules
  where
    patchRules =
      mappend $
        IntMap.fromList
          [ (8, CompoundRule [[42], [42, 8]]),
            (11, CompoundRule [[42, 31], [42, 11, 31]])
          ]

solve :: IntMap (Rule Int) -> [String] -> Int
solve rulesMap = count (any null . match rulesMap)

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
