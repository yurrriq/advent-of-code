module AdventOfCode.Year2019.Day02 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Arrow ((>>>), first)
import Data.List (find)
import Data.Vector ((!), Vector, fromList, modify, toList)
import qualified Data.Vector as V
import Data.Vector.Mutable (write)
import qualified Data.Vector.Mutable as MV
import Text.Trifecta (Parser, Result (..), comma, natural, parseString, sepBy)

program :: Parser (Vector Int)
program = fromList . map fromInteger <$> (natural `sepBy` comma)

main :: IO ()
main =
  do
    state <- parseInput program $(inputFilePath)
    putStr "Part One: "
    print $ partOne state
    putStr "Part Two: "
    print $ partTwo state

partOne :: Vector Int -> Int
partOne state = V.head (runProgram (restoreGravityAssist state))

partTwo :: Vector Int -> Int
partTwo state =
  maybe (error "Fail") (first (* 100) >>> uncurry (+)) $
    find go (concatMap (zip [0 .. n] . repeat) [0 .. n])
  where
    n = V.length state - 1
    go (noun, verb) =
      19690720 == V.head (runProgram (restoreGravityAssist' noun verb state))

restoreGravityAssist :: Vector Int -> Vector Int
restoreGravityAssist = restoreGravityAssist' 12 2

restoreGravityAssist' :: Int -> Int -> Vector Int -> Vector Int
restoreGravityAssist' noun verb =
  modify (\v -> write v 1 noun *> write v 2 verb)

runProgram :: Vector Int -> Vector Int
runProgram = go 0
  where
    go n state
      | state ! n == 99 = state
      | otherwise = go (n + 4) $ step (toList (V.slice n 4 state))
      where
        step [1, x, y, dst] = modify (runOp (+) x y dst) state
        step [2, x, y, dst] = modify (runOp (*) x y dst) state
        step _ = state
    runOp f x y dst v = write v dst =<< f <$> MV.read v x <*> MV.read v y

example1 :: Vector Int
example1 =
  case parseString program mempty "1,9,10,3,2,3,11,0,99,30,40,50" of
    Success prog -> prog
    Failure reason -> error (show reason)
