module AdventOfCode.Year2016.Day05
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.TH (inputFilePath)
import Crypto.Hash (Digest, MD5, hash)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.List (isPrefixOf)
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
    [ digest !! 5 | digest <- map show (digests doorID), "00000" `isPrefixOf` digest
    ]

partTwo :: String -> String
partTwo doorID =
  Map.elems . partTwo' Map.empty $
    [ (digest !! 5, digest !! 6) | digest <- map show (digests doorID), "00000" `isPrefixOf` digest, digest !! 5 `elem` "01234567"
    ]

partTwo' :: Password -> [(Char, Char)] -> Password
partTwo' m ((k, v) : kvs)
  | 8 == Map.size m = m
  | Map.member k m = partTwo' m kvs
  | otherwise = flip partTwo' kvs $ Map.insertWith (const id) k v m
partTwo' _ [] = error "333"

digests :: String -> [Digest MD5]
digests doorID = digests' (pack doorID) 0

digests' :: ByteString -> Integer -> [Digest MD5]
digests' doorID n = md5 (doorID <> pack (show n)) : digests' doorID (n + 1)

md5 :: ByteString -> Digest MD5
md5 = hash
