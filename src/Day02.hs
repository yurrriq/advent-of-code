{-# LANGUAGE OverloadedStrings #-}

module Day02 (
  partOne,
  partTwo
  ) where


import           Control.Arrow       ((***))
import           Data.ByteString     (ByteString)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM
import           Text.Trifecta       (Parser, Result (..), letter, newline,
                                      parseByteString, sepEndBy, some)


type BoxID = String

type Checksum = Integer

type Frequency a = HM.HashMap a Integer


boxID :: Parser BoxID
boxID = some letter


boxIDs :: Parser [BoxID]
boxIDs = boxID `sepEndBy` newline


frequencies :: (Eq a, Hashable a) => [a] -> Frequency a
frequencies = foldr go HM.empty
  where
    go :: (Eq a, Hashable a) => a -> Frequency a -> Frequency a
    go k = HM.insertWith (+) k 1


exactlyTwo :: (Eq a, Hashable a) => Frequency a -> Frequency a
exactlyTwo = HM.filter (==2)


exactlyThree :: (Eq a, Hashable a) => Frequency a -> Frequency a
exactlyThree = HM.filter (==3)


checksum :: [BoxID] -> Checksum
checksum = uncurry (*) . foldr go (0, 0)
  where
    go :: BoxID -> (Integer, Integer) -> (Integer, Integer)
    go bid =
        let freqs = frequencies bid
            f = if not (HM.null (exactlyTwo freqs)) then (+1) else id
            g = if not (HM.null (exactlyThree freqs)) then (+1) else id
        in f *** g


partOne :: ByteString -> Maybe Checksum
partOne input =
    case parseByteString boxIDs mempty input of
         Failure _errDoc -> Nothing
         Success ids     -> Just (checksum ids)


partTwo :: ByteString -> Maybe ()
partTwo = undefined
