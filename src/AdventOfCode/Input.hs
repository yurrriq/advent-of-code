{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Input
  ( parseInput,
    parseInputAoC,
    parseString,
    rawInput,
    rawInputAoC,
  )
where

import Advent (AoC (..), AoCUserAgent (..), defaultAoCOpts, mkDay_, runAoC_)
import Data.ByteString.UTF8 qualified as UTF8
import Paths_advent_of_code (getDataFileName)
import Relude
import System.Environment (getEnv)
import Text.Trifecta (Parser, Result (..), parseFromFileEx, runParser)
import Text.Trifecta qualified as Trifecta

parseInput :: Parser a -> FilePath -> IO a
parseInput parser fname =
  do
    fname' <- getDataFileName fname
    parsed <- parseFromFileEx parser fname'
    case parsed of
      Success result -> pure result
      Failure reason -> error (show reason)

parseInputAoC :: Integer -> Integer -> Parser a -> IO a
parseInputAoC year day parser = do
  opts <- defaultAoCOpts userAgent year <$> getEnv "AOC_SESSION_KEY"
  input <- toString <$> runAoC_ opts (AoCInput (mkDay_ day))
  case runParser parser mempty input of
    Success result -> pure result
    Failure reason -> error (show reason)

parseString :: (MonadIO m) => Parser a -> String -> m a
parseString parser =
  Trifecta.parseString parser mempty >>> \case
    Success result -> pure result
    Failure reason -> error (show reason)

rawInput :: FilePath -> IO String
rawInput = getDataFileName >=> readFileBS >>> fmap UTF8.toString

rawInputAoC :: Integer -> Integer -> IO String
rawInputAoC year day = do
  opts <- defaultAoCOpts userAgent year <$> getEnv "AOC_SESSION_KEY"
  toString <$> runAoC_ opts (AoCInput (mkDay_ day))

userAgent :: AoCUserAgent
userAgent = AoCUserAgent "github.com/yurrriq/advent-of-code" "eric@ericb.me"
