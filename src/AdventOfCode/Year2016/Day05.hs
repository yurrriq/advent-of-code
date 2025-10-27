{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Year2016.Day05
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.TH (defaultMain, inputFilePath)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BSL
import Data.List.Extra (nubOrdOn, sort)

main :: IO ()
main = $(defaultMain)

getInput :: IO Builder
getInput = Builder.byteString . BS.take 8 <$> BS.readFile $(inputFilePath)

partOne :: Builder -> ByteString
partOne doorID =
  BS.pack . take 8 $
    [ digest `BS.index` 5
      | digest <- digests doorID,
        "00000" `BS.isPrefixOf` digest
    ]

partTwo :: Builder -> ByteString
partTwo doorID =
  BS.pack . map snd . sort . take 8 . nubOrdOn fst $
    [ (digest `BS.index` 5, digest `BS.index` 6)
      | digest <- digests doorID,
        "00000" `BS.isPrefixOf` digest,
        digest `BS.index` 5 `BS.elem` "01234567"
    ]

digests :: Builder -> [ByteString]
digests doorID = md5 . flip appendInt doorID <$> [0 ..]

md5 :: ByteString -> ByteString
md5 = Base16.encode . MD5.hash

-- | Append the decimal encoding of an 'Int' to a 'Builder' to generate a
-- 'ByteString'.
appendInt :: Int -> Builder -> ByteString
appendInt n salt =
  BSL.toStrict $
    Builder.toLazyByteString $
      salt <> Builder.intDec n
