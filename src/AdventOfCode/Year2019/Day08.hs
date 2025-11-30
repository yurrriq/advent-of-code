{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2019.Day08 where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.Util qualified as Util
import Data.List.Extra (minimumBy, sumOn')
import Data.Text.IO qualified as TextIO
import Relude
import Text.Show qualified
import Text.Trifecta (Parser, char, choice, count, (<?>))

data Pixel
  = Black
  | White
  | Transparent
  deriving (Eq, Generic)

instance Semigroup Pixel where
  Transparent <> below = below
  above <> _ = above

instance Monoid Pixel where
  mempty = Transparent

instance Show Pixel where
  show Black = " "
  show White = "#"
  show Transparent = "."

type Image = [Layer]

type Layer = [Row]

type Row = [Pixel]

main :: IO ()
main =
  getInput >>= evaluatingPuzzle do
    putStr "Part One: "
    print =<< partOne
    putStrLn "Part Two:"
    liftIO . TextIO.putStrLn =<< partTwo

getInput :: IO Image
getInput = parseInputAoC 2019 8 (image 25 6)

partOne :: SimplePuzzle Image Int
partOne =
  asks
    $ minimumBy (compare `on` numberOf Black)
    >>> (numberOf White &&& numberOf Transparent)
    >>> uncurry (*)
  where
    numberOf x = sumOn' (Util.count (== x))

partTwo :: SimplePuzzle Image Text
partTwo =
  asks
    $ foldl' decodeLayer (transparentLayer 25 6)
    >>> map (foldMap show)
    >>> unlines
  where
    decodeLayer = zipWith (zipWith (<>))

image :: Int -> Int -> Parser Image
image width height = some (count height (count width pixel))

pixel :: Parser Pixel
pixel =
  choice
    [ char '0' *> pure Black <?> "A black pixel",
      char '1' *> pure White <?> "A white pixel",
      char '2' *> pure Transparent <?> "A transparent pixel"
    ]

transparentLayer :: Int -> Int -> Layer
transparentLayer width height = replicate height (replicate width Transparent)
