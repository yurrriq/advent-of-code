{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Year2016.Day14 where

import Control.Applicative ((<|>))
import Control.Lens (makeLenses, use, uses, (%=))
import Control.Monad.Extra (anyM, guard, ifM, notM, (>=>))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, MonadLogger, logDebug, runStderrLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Crypto.Hash.MD5 qualified as MD5
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
  {runPuzzle :: ReaderT Builder (StateT PuzzleState (LoggingT IO)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadLogger,
      MonadState PuzzleState,
      MonadReader Builder,
      MonadFail
    )

data PuzzleState = PuzzleState
  { _index :: Int,
    _digests :: IntMap ByteString
  }

$(makeLenses ''PuzzleState)

main :: IO ()
main = do
  salt <- getInput
  putStr "Part One: "
  print =<< partOne salt
  putStr "Part Two: "
  print =<< partTwo salt

getInput :: IO ByteString
getInput = pure "yjdafjpo"

partOne :: ByteString -> IO Int
partOne = flip partOne' 64

partOne' :: ByteString -> Int -> IO Int
partOne' = solveWith md5

partTwo :: ByteString -> IO Int
partTwo = flip partTwo' 64

partTwo' :: ByteString -> Int -> IO Int
partTwo' = solveWith \string ->
  iterate md5 string !! 2017

solveWith :: (ByteString -> ByteString) -> ByteString -> Int -> IO Int
solveWith hash = solve go
  where
    go 0 = fail "Must specify a strictly positive number"
    go 1 = nextKey
    go k = nextKey *> go (k - 1)

    nextKey = nextKeyWith hashFor hasRun
    hashFor = hashForWith hash
    hasRun = hasRunWith hashFor

solve :: (Int -> Puzzle Int) -> ByteString -> Int -> IO Int
solve loop salt =
  runStderrLoggingT
    . flip evalStateT (PuzzleState {_index = 0, _digests = IntMap.empty})
    . flip runReaderT (Builder.byteString salt)
    . runPuzzle
    . loop

-- | Find the next index that produces a key using given hash and run detection
-- functions.
nextKeyWith :: (Int -> Puzzle ByteString) -> (Int -> Word8 -> Int -> Puzzle Bool) -> Puzzle Int
nextKeyWith hashFor hasRun = do
  digest <- use index >>= hashFor
  i <- use index
  incrementIndex
  let loop = nextKeyWith hashFor hasRun
  flip (maybe loop) (hasTriple digest) \digit ->
    ifM (notM (anyM (hasRun 5 digit) [i + 1 .. i + 1000])) loop do
      $(logDebug) (fromString (printf "Found key with %d" i))
      pure i

-- | Determine whether the hash for an integer index contains @k@ of a given
-- character in a row.
hasRunWith :: (Int -> Puzzle ByteString) -> Int -> Word8 -> Int -> Puzzle Bool
hasRunWith hashFor k character =
  fmap (BS.replicate k character `BS.isInfixOf`) . hashFor

-- | Append an integer index to the salt and compute the hash using a given
-- algorithm.
hashForWith :: (ByteString -> ByteString) -> Int -> Puzzle ByteString
hashForWith go i = do
  string <- asks (appendInt i)
  digests %= IntMap.alter (<|> Just (go string)) i
  uses digests (! i)

-- | Compute the hexademical representation of the MD5 hash of a 'ByteString'.
md5 :: ByteString -> ByteString
md5 = Base16.encode . MD5.hash

-- | Determine whether a 'ByteString' contains three of the same character in a
-- row.
hasTriple :: ByteString -> Maybe Word8
hasTriple =
  BS.uncons >=> \(character, characters) ->
    character <$ guard (BS.replicate 2 character `BS.isPrefixOf` characters)
      <|> hasTriple characters

-- | Append the decimal encoding of an 'Int' to a 'Builder' to generate a
-- 'ByteString'.
appendInt :: Int -> Builder -> ByteString
appendInt n salt =
  BSL.toStrict $
    Builder.toLazyByteString $
      salt <> Builder.intDec n

-- | Increment the integer index.
incrementIndex :: Puzzle ()
incrementIndex = index %= (+ 1)
