module AdventOfCode.Year2021.Day06 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Monad (forM_)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Linear (Additive, (!*), (!*!))
import Text.Trifecta (commaSep, natural)

-- TODO: Data.Finite
-- TODO: Data.Vector.Sized

main :: IO ()
main = $(defaultMain)

getInput :: IO [Int]
getInput = parseInput (commaSep (fromInteger <$> natural)) $(inputFilePath)

example :: [Int]
example = [3, 4, 3, 1, 2]

partOne :: [Int] -> Int
partOne = simulate 80

partTwo :: [Int] -> Int
partTwo = simulate 256

simulate :: Int -> [Int] -> Int
simulate n xs = sum $ (matrix !^! n) !* v
  where
    v = V.modify (forM_ xs . flip MV.modify succ) (V.replicate 9 0)

matrix :: V.Vector (V.Vector Int)
matrix =
  V.fromListN 9 . map (V.fromListN 9) $
    [ [0, 1, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 1, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 1, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 1, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 1, 0, 0],
      [1, 0, 0, 0, 0, 0, 0, 1, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 1],
      [1, 0, 0, 0, 0, 0, 0, 0, 0]
    ]

infixl 8 !^!

(!^!) :: (Foldable m, Additive m, Num a) => m (m a) -> Int -> m (m a)
a !^! n = iterate (!*! a) a !! pred n

-- TODO: stimes with Semigroup
