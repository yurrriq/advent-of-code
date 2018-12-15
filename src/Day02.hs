module Day02
  ( partOne
  , partTwo
  ) where

import           Control.Arrow   ((&&&), (***), (>>>))
import           Control.Monad   ((>=>))
import           Data.ByteString (ByteString)
import           Data.List       (find, tails)
import           Data.Maybe      (listToMaybe, mapMaybe)
import           Text.Trifecta   (Parser, letter, newline, sepEndBy, some)
import           Util            (commonElems, frequencies, hammingSimilar,
                                  maybeParseByteString)

type BoxID = String

boxID :: Parser BoxID
boxID = some letter

boxIDs :: Parser [BoxID]
boxIDs = boxID `sepEndBy` newline

type Checksum = Integer

checksum :: [BoxID] -> Integer
checksum = fmap frequencies >>>
           filter (elem 2) &&& filter (elem 3) >>>
           length *** length >>>
           product >>>
           fromIntegral

partOne :: ByteString -> Maybe Checksum
partOne = maybeParseByteString boxIDs >=> pure . checksum

correctBoxIDs :: [BoxID] -> Maybe (BoxID, BoxID)
correctBoxIDs = listToMaybe . mapMaybe go . tails
  where
    go :: [BoxID] -> Maybe (BoxID, BoxID)
    go (x:xs@(_:_)) = (,) <$> pure x <*> find (hammingSimilar 1 x) xs
    go _            = Nothing

partTwo :: ByteString -> Maybe String
partTwo = maybeParseByteString boxIDs >=>
          correctBoxIDs >=>
          uncurry commonElems
