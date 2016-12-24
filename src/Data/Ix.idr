-- ------------------------------------------------------------------ [ Ix.idr ]
-- Module      : Data.Ix
-- Description : A partial port of Haskell's Data.Ix to Idris.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : https://hackage.haskell.org/package/base/docs/Data-Ix.html
-- --------------------------------------------------------------------- [ EOH ]
||| A partial port of Haskell's Data.Ix to Idris.
module Data.Ix

import Debug.Error

%language ElabReflection

-- ------------------------------------------------------------------ [ Errors ]

%access export

indexError : Show a => (a,a) -> a -> String -> b
indexError rng i tp =
    error $ "Ix{" ++ show tp ++ "}.index: Index " ++
            "(" ++ show i ++ ") out of range " ++
            "(" ++ show rng ++ ")"

hopelessIndexError : Int -- Try to use 'indexError' instead!
hopelessIndexError = error "Error in array index"

-- --------------------------------------------------------------- [ Interface ]

%access public export

interface (Ord a) => Ix a where
  range                   : (a,a) -> List a
  index                   : (a,a) -> a -> Int
  unsafeIndex             : (a,a) -> a -> Int
  inRange                 : (a,a) -> a -> Bool
  rangeSize               : (a,a) -> Int
  unsafeRangeSize         : (a,a) -> Int

  index b i               = if inRange b i
                               then unsafeIndex b i
                               else hopelessIndexError

  unsafeIndex b i         = index b i

  rangeSize b@(_,h)       = if inRange b h
                               then unsafeIndex b h + 1
                               else 0               -- This case is only here to
                                                    -- check for an empty range

  unsafeRangeSize b@(_,h) = unsafeIndex b h + 1

-- --------------------------------------------------------- [ Implementations ]

implementation Ix Nat where
  range (m,n)             = [m .. n]
  unsafeIndex (m,_) i     = cast $ i `minus` m
  index b i               = if inRange b i
                               then unsafeIndex b i
                               else indexError b i "Nat"
  inRange (m,n) i         = m <= i && i <= n

-- --------------------------------------------------------------------- [ EOF ]
