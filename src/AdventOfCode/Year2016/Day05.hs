{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Year2016.Day05
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.TH (inputFilePath)
import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map.Lazy qualified as Map

type Password = Map.Map Char Char

main :: IO ()
main = do
  doorID <- head . words <$> readFile $(inputFilePath)
  putStr "Part One: "
  putStrLn (partOne doorID)
  putStr "Part Two: "
  putStrLn (partTwo doorID)

partOne :: String -> String
partOne doorID =
  take 8 $
    [ digest `BS.index` 5
      | digest <- digests doorID,
        "00000" `BS.isPrefixOf` digest
    ]

partTwo :: String -> String
partTwo doorID =
  Map.elems . partTwo' Map.empty $
    [ (digest `BS.index` 5, digest `BS.index` 6)
      | digest <- digests doorID,
        "00000" `BS.isPrefixOf` digest,
        digest `BS.index` 5 `BS.elem` "01234567"
    ]

partTwo' :: Password -> [(Char, Char)] -> Password
partTwo' m ((k, v) : kvs)
  | 8 == Map.size m = m
  | Map.member k m = partTwo' m kvs
  | otherwise = flip partTwo' kvs $ Map.insertWith (const id) k v m
partTwo' _ [] = error "333"

digests :: String -> [ByteString]
digests doorID = digests' (BS.pack doorID) 0

digests' :: ByteString -> Integer -> [ByteString]
digests' doorID n = md5 (doorID <> BS.pack (show n)) : digests' doorID (n + 1)

md5 :: ByteString -> ByteString
md5 = Base16.encode . hash
