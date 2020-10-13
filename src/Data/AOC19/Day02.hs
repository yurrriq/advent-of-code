module Data.AOC19.Day02 where

import Control.Arrow ((>>>), first)
import Data.List (find)
import Data.Vector ((!), Vector, fromList, modify, toList)
import qualified Data.Vector as V
import Data.Vector.Mutable (write)
import qualified Data.Vector.Mutable as MV
import Text.Trifecta
  ( Parser,
    Result (..),
    comma,
    natural,
    parseFromFile,
    parseString,
    sepBy,
  )

program :: Parser (Vector Int)
program = fromList . map fromInteger <$> (natural `sepBy` comma)

partOne :: IO Int
partOne =
  do
    res <- parseFromFile program "../../../input/day02.txt"
    case res of
      Nothing -> error "No parse"
      Just state -> pure (V.head (runProgram (restoreGravityAssist state)))

partTwo :: IO Int
partTwo =
  do
    res <- parseFromFile program "../../../input/day02.txt"
    case res of
      Nothing -> error "No parse"
      Just state ->
        do
          let n = V.length state - 1
          pure . maybe (error "Fail") (first (* 100) >>> uncurry (+)) $
            find (go state) (concatMap (zip [0 .. n] . repeat) [0 .. n])
  where
    go state (noun, verb) =
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
