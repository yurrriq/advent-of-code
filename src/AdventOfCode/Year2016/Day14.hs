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
import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BSL
import Data.IntMap (IntMap, (!))
import Data.IntMap qualified as IntMap
import Data.String (fromString)
import Data.Word (Word8)
import Text.Printf (printf)

newtype Puzzle a = Puzzle
  {runPuzzle :: ReaderT Builder (StateT (Int, IntMap ByteString) (LoggingT IO)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadLogger,
      MonadState (Int, IntMap ByteString),
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

md5 :: Puzzle ByteString
md5 = md5With =<< gets fst

md5With :: Int -> Puzzle ByteString
md5With i = do
  string <- asks (appendInt i)
  let go = (<|> Just (Base16.encode (hash string)))
  _2 %= IntMap.alter go i
  uses _2 (! i)

appendInt :: Int -> Builder -> ByteString
appendInt n salt =
  BSL.toStrict $
    Builder.toLazyByteString $
      salt <> Builder.intDec n

hasTriple :: ByteString -> Maybe Word8
hasTriple =
  BS.uncons >=> \(c, cs) ->
    if BS.replicate 2 c `BS.isPrefixOf` cs
      then Just c
      else hasTriple cs

hasRun :: Int -> Word8 -> Int -> Puzzle Bool
hasRun k digit = fmap (BS.replicate k digit `BS.isInfixOf`) . md5With
