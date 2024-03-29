{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Input where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Paths_advent_of_code (getDataFileName)
import Text.Trifecta (Parser, Result (..), parseFromFileEx)
import qualified Text.Trifecta as Trifecta

parseInput :: Parser a -> FilePath -> IO a
parseInput parser fname =
  do
    fname' <- getDataFileName fname
    parsed <- parseFromFileEx parser fname'
    case parsed of
      Success result -> pure result
      Failure reason -> error (show reason)

parseString :: Parser a -> String -> IO a
parseString parser =
  Trifecta.parseString parser mempty >>> \case
    Success result -> pure result
    Failure reason -> error (show reason)

rawInput :: FilePath -> IO String
rawInput = getDataFileName >=> readFile
