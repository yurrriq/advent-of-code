module Util
  ( Frequencies
  , frequencies
  ) where

import           Data.Hashable       (Hashable (..))
import qualified Data.HashMap.Strict as HM


type Frequencies a = HM.HashMap a Integer


frequencies :: (Eq a, Hashable a) => [a] -> Frequencies a
frequencies = foldr go HM.empty
  where
    go :: (Eq a, Hashable a) => a -> Frequencies a -> Frequencies a
    go k = HM.insertWith (+) k 1
