module Data.AOC19.Day02 where

import           Data.Vector         (Vector, fromList, modify, toList, (!))
import qualified Data.Vector         as V
import           Data.Vector.Mutable (write)
import qualified Data.Vector.Mutable as MV
import           Text.Trifecta       (Parser, Result (..), comma, natural,
                                      parseFromFile, parseString, sepBy)


program :: Parser (Vector Int)
program = fromList . map fromInteger <$> (natural `sepBy` comma)


partOne :: IO Int
partOne =
    do res <- parseFromFile program "../../../input/day02.txt"
       case res of
         Just state -> pure (V.head (runProgram (restoreGravityAssist state)))
         Nothing    -> error "No parse"


restoreGravityAssist :: Vector Int -> Vector Int
restoreGravityAssist = modify go
  where
    go v = do write v 1 12
              write v 2 2


runProgram :: Vector Int -> Vector Int
runProgram = go 0
  where
    go n state
      | state ! n == 99 = state
      | otherwise       = go (n + 4) $ step (toList (V.slice n 4 state))
      where
        step [1,x,y,dst] = modify (runOp (+) x y dst) state
        step [2,x,y,dst] = modify (runOp (*) x y dst) state
        step _           = state

    runOp f x y dst v = write v dst =<< f <$> MV.read v x <*> MV.read v y


example1 :: Vector Int
example1 =
    case parseString program mempty "1,9,10,3,2,3,11,0,99,30,40,50" of
      Success prog   -> prog
      Failure reason -> error (show reason)
