{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2016.Day14 where

import AdventOfCode.Puzzle
import AdventOfCode.TH (evalPuzzle)
import Control.Lens (makeLenses, use, uses, (%=))
import Control.Monad.Extra (notM)
import Control.Monad.Logger (logDebug)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BSL
import Data.IntMap ((!))
import Data.IntMap qualified as IntMap
import Relude
import Relude.Unsafe ((!!))
import Text.Printf (printf)

data PuzzleState = PuzzleState
  { _index :: !Int,
    _digests :: !(IntMap ByteString)
  }
  deriving (Generic, Show)

makeLenses ''PuzzleState

emptyPuzzleState :: PuzzleState
emptyPuzzleState = PuzzleState 0 IntMap.empty

main :: IO ()
main = $(evalPuzzle)

getInput :: IO Builder
getInput = pure "yjdafjpo"

partOne :: Puzzle Builder PuzzleState Int
partOne = partOne' 64

partOne' :: Int -> Puzzle Builder PuzzleState Int
partOne' = solveWith md5

partTwo :: Puzzle Builder PuzzleState Int
partTwo = partTwo' 64

partTwo' :: Int -> Puzzle Builder PuzzleState Int
partTwo' = solveWith \string ->
  iterate md5 string !! 2017

solveWith :: (ByteString -> ByteString) -> Int -> Puzzle Builder PuzzleState Int
solveWith hash = \case
  0 -> fail "Must specify a strictly positive number"
  1 -> nextKey
  k -> nextKey *> solveWith hash (k - 1)
  where
    nextKey = nextKeyWith hashFor hasRun
    hashFor = hashForWith hash
    hasRun = hasRunWith hashFor

-- | Find the next index that produces a key using given hash and run detection
-- functions.
nextKeyWith :: (Int -> Puzzle Builder PuzzleState ByteString) -> (Int -> Word8 -> Int -> Puzzle Builder PuzzleState Bool) -> Puzzle Builder PuzzleState Int
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
hasRunWith :: (Int -> Puzzle Builder PuzzleState ByteString) -> Int -> Word8 -> Int -> Puzzle Builder PuzzleState Bool
hasRunWith hashFor k character =
  fmap (BS.replicate k character `BS.isInfixOf`) . hashFor

-- | Append an integer index to the salt and compute the hash using a given
-- algorithm.
hashForWith :: (ByteString -> ByteString) -> Int -> Puzzle Builder PuzzleState ByteString
hashForWith hash i = do
  string <- asks (appendInt i)
  digests %= IntMap.alter (<|> Just (hash string)) i
  uses digests (! i)

-- | Compute the hexademical representation of the MD5 hash of a 'ByteString'.
md5 :: ByteString -> ByteString
md5 = Base16.encode . MD5.hash

-- | Determine whether a 'ByteString' contains three of the same character in a
-- row.
hasTriple :: ByteString -> Maybe Word8
hasTriple =
  BS.uncons >=> \(character, characters) ->
    character
      <$ guard (BS.replicate 2 character `BS.isPrefixOf` characters)
      <|> hasTriple characters

-- | Append the decimal encoding of an 'Int' to a 'Builder' to generate a
-- 'ByteString'.
appendInt :: Int -> Builder -> ByteString
appendInt n salt =
  BSL.toStrict
    $ Builder.toLazyByteString
    $ salt
    <> Builder.intDec n

-- | Increment the integer index.
incrementIndex :: (MonadState PuzzleState m) => m ()
incrementIndex = index %= (+ 1)
