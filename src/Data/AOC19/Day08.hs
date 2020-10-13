-- ---------------------------------------------------------------- [ Day08.hs ]
-- TODO: Module doc
-- --------------------------------------------------------------------- [ EOH ]

module Data.AOC19.Day08
  ( main,
    partOne,
    partTwo,
  )
where

import Control.Applicative ((<|>))
import Data.Function (on)
import Data.List (sortBy)
import System.Environment (getArgs)
import Text.Trifecta
  ( (<?>),
    Parser,
    char,
    count,
    parseFromFile,
    some,
  )

-- ------------------------------------------------------------------- [ Types ]

data Pixel
  = Black
  | White
  | Transparent
  deriving (Enum, Eq)

instance Show Pixel where
  show Black = " "
  show White = "#"
  show Transparent = "."

type Image = [Layer]

type Layer = [Row]

type Row = [Pixel]

-- -------------------------------------------------------------------- [ Main ]

main :: IO ()
main =
  do
    putStr "Part One: "
    partOne =<< getInputFilename
    putStrLn "Part Two: "
    partTwo =<< getInputFilename

-- ---------------------------------------------------------------- [ Part One ]

partOne :: FilePath -> IO ()
partOne fname =
  do
    Just layers <- parseFromFile (image 25 6) fname
    let layer = head $ sortBy (compare `on` numberOf Black) layers
    let ones = numberOf White layer
    let twos = numberOf Transparent layer
    print $ ones * twos
  where
    numberOf :: Eq a => a -> [[a]] -> Int
    numberOf x = sum . fmap (length . filter (== x))

-- ---------------------------------------------------------------- [ Part Two ]

partTwo :: FilePath -> IO ()
partTwo fname =
  do
    Just layers <- parseFromFile (image 25 6) fname
    putStrLn
      $ unlines . map (concatMap show)
      $ foldl decodeLayer (transparentLayer 25 6) layers
  where
    decodeLayer :: Layer -> Layer -> Layer
    decodeLayer = zipWith (zipWith decodePixel)
    decodePixel :: Pixel -> Pixel -> Pixel
    decodePixel Transparent below = below
    decodePixel above _ = above

-- ----------------------------------------------------------------- [ Parsers ]

image :: Int -> Int -> Parser Image
image width height = some layer
  where
    layer :: Parser Layer
    layer = count height row
    row :: Parser Row
    row = count width pixel

pixel :: Parser Pixel
pixel =
  (char '0' *> pure Black <?> "A black pixel")
    <|> (char '1' *> pure White <?> "A white pixel")
    <|> (char '2' *> pure Transparent <?> "A transparent pixel")

-- ----------------------------------------------------------------- [ Helpers ]

transparentLayer :: Int -> Int -> Layer
transparentLayer width height = replicate height (replicate width Transparent)

getInputFilename :: IO FilePath
getInputFilename =
  do
    args <- getArgs
    case args of
      [fname] -> pure fname
      [] -> error "Must specify input filename"
      _ -> error "Too many args"
-- --------------------------------------------------------------------- [ EOF ]
