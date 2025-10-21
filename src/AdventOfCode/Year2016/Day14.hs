{-# LANGUAGE BlockArguments #-}

module AdventOfCode.Year2016.Day14 where

import AdventOfCode.TH (defaultMain)
import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.Lens (over, uses, (%=), (%~), (+~), _1, _2, _3)
import Control.Monad.Extra (anyM, liftM, when)
import Control.Monad.State (MonadIO, State, StateT, evalStateT, get, gets, liftIO, modify, put)
import Crypto.Hash (Digest, MD5, hash)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntMap (IntMap (..), (!))
import Data.IntMap qualified as IntMap
import Data.List (find, group, isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.String (fromString)

main :: IO ()
main =
  do
    salt <- getInput
    putStr "Part One: "
    print =<< partOne salt
    putStr "Part Two: "
    print =<< partTwo salt

partOne :: ByteString -> IO Int
partOne salt = evalStateT (go 0 64) (salt, IntMap.empty)
  where
    go i 0 = pure (i - 1)
    go i k =
      do
        n <- nextKey i
        go (n + 1) (k - 1)

partTwo :: ByteString -> IO Int
partTwo = undefined

getInput :: IO ByteString
getInput = pure (fromString "yjdafjpo")

nextKey :: (MonadIO m) => Int -> StateT (ByteString, IntMap String) m Int
nextKey i =
  do
    (salt, digests) <- get
    digest <- md5With i
    case producesTriple digest of
      Nothing -> nextKey (i + 1)
      Just digit ->
        do
          isKey <- anyM (hasRun 5 digit) [i + 1 .. i + 1000]
          if isKey
            then pure i
            else nextKey (i + 1)

md5With :: (MonadIO m) => Int -> StateT (ByteString, IntMap String) m String
md5With i =
  do
    (salt, digests) <- get
    let go = (<|> Just (show (hash @ByteString @MD5 (salt `BS.append` fromString (show i)))))
    _2 %= IntMap.alter go i
    uses _2 (! i)

producesTriple :: String -> Maybe Char
producesTriple = go
  where
    go [] = Nothing
    go (c : cs)
      | replicate 2 c `isPrefixOf` cs = Just c
      | otherwise = go cs

hasRun :: (MonadIO m) => Int -> Char -> Int -> StateT (ByteString, IntMap String) m Bool
hasRun k digit = fmap (replicate k digit `isInfixOf`) . md5With
