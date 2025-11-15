{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2018.Day03 where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.Puzzle
import AdventOfCode.TH (evalPuzzle)
import AdventOfCode.Util (Frequencies, frequencies, maybeFail)
import Control.Monad.Extra (findM)
import Data.Ix (range)
import Data.Map qualified as Map
import Linear.V2 (V2 (..))
import Relude
import Text.Trifecta (Parser, comma, natural, symbol)

-- ------------------------------------------------------------------  [ Types ]

data Claim = Claim
  { _id :: !Integer,
    _origin :: !(V2 Integer),
    _size :: !(V2 Integer)
  }
  deriving (Eq, Show)

-- ----------------------------------------------------------------- [ Parsers ]

parseClaim :: Parser Claim
parseClaim =
  Claim
    <$> (symbol "#" *> natural)
    <*> (symbol "@" *> point)
    <*> (symbol ":" *> size)
  where
    point = V2 <$> natural <*> (comma *> natural)
    size = V2 <$> natural <*> (symbol "x" *> natural)

-- ----------------------------------------------------------------- [ Helpers ]

nonoverlappingClaim :: Claim -> Puzzle [Claim] (Frequencies (V2 Integer)) Bool
nonoverlappingClaim =
  liftA2 all (gets nonoverlappingSquare) . pure . squaresCovered

nonoverlappingSquare :: Frequencies (V2 Integer) -> V2 Integer -> Bool
nonoverlappingSquare covered square =
  Map.lookup square covered == Just 1

squaresCovered :: Claim -> [V2 Integer]
squaresCovered (Claim _ origin size) = range (origin, origin + size - 1)

-- ------------------------------------------------------------------- [ Parts ]

emptyPuzzleState :: Frequencies (V2 Integer)
emptyPuzzleState = Map.empty

partOne :: Puzzle [Claim] (Frequencies (V2 Integer)) Int
partOne =
  asks
    $ Map.size
    . Map.filter (>= 2)
    . frequencies
    . concatMap squaresCovered

partTwo :: Puzzle [Claim] (Frequencies (V2 Integer)) Integer
partTwo = do
  put =<< asks (frequencies . concatMap squaresCovered)
  maybeFail "could not find nonoverlapping claim"
    . fmap _id
    =<< findM nonoverlappingClaim
    =<< ask

getInput :: IO [Claim]
getInput = parseInputAoC 2018 3 (many parseClaim)

main :: IO ()
main = $(evalPuzzle)

getExample :: IO [Claim]
getExample = parseString (many parseClaim) example

example :: String
example =
  "#1 @ 1,3: 4x4\n\
  \#2 @ 3,1: 4x4\n\
  \#3 @ 5,5: 2x2\n"
