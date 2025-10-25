{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Year2016.Day14 where

import Control.Applicative ((<|>))
import Control.Lens (uses, (%=), _1, _2)
import Control.Monad.Extra (anyM, ifM, notM, (>=>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebug, runStderrLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, gets)
import Crypto.Hash (MD5 (MD5), hashWith)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BSL
import Data.IntMap (IntMap, (!))
import Data.IntMap qualified as IntMap
import Data.List (isInfixOf, isPrefixOf, uncons)
import Data.String (fromString)
import Text.Printf (printf)

newtype Puzzle a = Puzzle
  {runPuzzle :: ReaderT Builder (StateT (Int, IntMap String) (LoggingT IO)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadLogger,
      MonadState (Int, IntMap String),
      MonadReader Builder,
      MonadFail
    )

main :: IO ()
main = do
  salt <- getInput
  putStr "Part One: "
  print =<< partOne salt
  putStr "Part Two: "
  print =<< partTwo salt

partOne :: ByteString -> IO Int
partOne = flip partOne' 64

partOne' :: ByteString -> Int -> IO Int
partOne' salt =
  runStderrLoggingT
    . flip evalStateT (0, IntMap.empty)
    . flip runReaderT (Builder.byteString salt)
    . runPuzzle
    . go
  where
    go 0 = fail "Must specify a strictly positive number"
    go 1 = nextKey
    go k = nextKey *> go (k - 1)

partTwo :: ByteString -> IO Int
partTwo = undefined

getInput :: IO ByteString
getInput = pure "yjdafjpo"

nextKey :: Puzzle Int
nextKey = do
  digest <- md5
  i <- gets fst
  _1 %= (+ 1)
  case hasTriple digest of
    Nothing -> nextKey
    Just digit ->
      ifM (notM (anyM (hasRun 5 digit) [i + 1 .. i + 1000])) nextKey do
        $(logDebug) (fromString (printf "Found key with %d" i))
        pure i

md5 :: Puzzle String
md5 = md5With =<< gets fst

md5With :: Int -> Puzzle String
md5With i = do
  string <- asks (appendInt i)
  let go = (<|> Just (show (hashWith MD5 string)))
  _2 %= IntMap.alter go i
  uses _2 (! i)

appendInt :: Int -> Builder -> ByteString
appendInt n salt =
  BSL.toStrict $
    Builder.toLazyByteString $
      salt <> Builder.intDec n

hasTriple :: String -> Maybe Char
hasTriple =
  uncons >=> \(c, cs) ->
    if replicate 2 c `isPrefixOf` cs
      then Just c
      else hasTriple cs

hasRun :: Int -> Char -> Int -> Puzzle Bool
hasRun k digit = fmap (replicate k digit `isInfixOf`) . md5With
