{-# LANGUAGE LambdaCase #-}


module Util
  ( Frequencies
  , frequencies
  , maybeParseByteString
  ) where


import           Control.Category    ((>>>))
import           Data.ByteString     (ByteString)
import           Data.Hashable       (Hashable (..))
import qualified Data.HashMap.Strict as HM
import           Text.Trifecta       (Parser, Result (..), parseByteString)


type Frequencies a = HM.HashMap a Integer


frequencies :: (Eq a, Hashable a) => [a] -> Frequencies a
frequencies = foldr go HM.empty
  where
    go :: (Eq a, Hashable a) => a -> Frequencies a -> Frequencies a
    go k = HM.insertWith (+) k 1


maybeParseByteString :: Parser a -> ByteString -> Maybe a
maybeParseByteString p = parseByteString p mempty >>> \case
  Failure _   -> Nothing
  Success res -> Just res
