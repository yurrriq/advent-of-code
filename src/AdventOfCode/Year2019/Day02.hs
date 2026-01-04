{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2019.Day02 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util (maybeFail)
import Data.Function.Pointless ((.:))
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Relude
import Text.Trifecta (Parser, comma, natural, sepBy)
import Prelude (until)

program :: Parser (Vector Int)
program = V.fromList . map fromInteger <$> (natural `sepBy` comma)

getInput :: IO (Vector Int)
getInput = parseInputAoC 2019 2 program

main :: IO ()
main = $(defaultMainPuzzle)

partOne :: SimplePuzzle (Vector Int) Int
partOne = asks (V.head . runProgram . restoreGravityAssist 12 2)

partTwo :: SimplePuzzle (Vector Int) Int
partTwo = do
  n <- asks (subtract 1 . V.length)
  go <- asks (runProgram .: restoringGravityAssist)
  (noun, verb) <-
    maybeFail "could not find matching input"
      $ find ((19690720 ==) . V.head . go)
      $ concatMap (zip [0 .. n] . repeat) [0 .. n]
  pure (100 * noun + verb)

restoringGravityAssist :: Vector Int -> (Int, Int) -> Vector Int
restoringGravityAssist prog (noun, verb) = restoreGravityAssist noun verb prog

restoreGravityAssist :: Int -> Int -> Vector Int -> Vector Int
restoreGravityAssist noun verb =
  V.modify (\v -> MV.write v 1 noun *> MV.write v 2 verb)

runProgram :: Vector Int -> Vector Int
runProgram = fst . until ((99 ==) . uncurry (!)) go . (,0)
  where
    go (prog, n) = (step (V.toList (V.slice n 4 prog)), n + 4)
      where
        step [1, x, y, dst] = V.modify (runOp (+) x y dst) prog
        step [2, x, y, dst] = V.modify (runOp (*) x y dst) prog
        step _ = prog
    runOp f x y dst v = MV.write v dst =<< f <$> MV.read v x <*> MV.read v y

example1 :: IO (Vector Int)
example1 = parseString program "1,9,10,3,2,3,11,0,99,30,40,50"
