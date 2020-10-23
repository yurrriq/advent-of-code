module Day01
  ( partOne,
    partTwo,
  )
where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.Hashable (Hashable (..))
import Data.Monoid (Sum (..))
import Text.Trifecta (Parser, integer, many)
import Util (findFirstDup, maybeParseByteString, scan)

newtype FrequencyChange
  = FrequencyChange
      {unFrequencyChange :: Sum Integer}
  deriving (Eq, Show)

instance Hashable FrequencyChange where
  hashWithSalt salt = hashWithSalt salt . getSum . unFrequencyChange

instance Semigroup FrequencyChange where
  (FrequencyChange x) <> (FrequencyChange y) = FrequencyChange (x <> y)

instance Monoid FrequencyChange where
  mempty = FrequencyChange (Sum 0)

frequencyChanges :: Parser [FrequencyChange]
frequencyChanges = many (FrequencyChange . Sum <$> integer)

partOne :: ByteString -> Maybe Integer
partOne =
  fmap (getSum . unFrequencyChange . mconcat)
    . maybeParseByteString frequencyChanges

partTwo :: ByteString -> Maybe Integer
partTwo =
  maybeParseByteString frequencyChanges
    >=> scan . cycle
    >>> findFirstDup
    >>> fmap (getSum . unFrequencyChange)
