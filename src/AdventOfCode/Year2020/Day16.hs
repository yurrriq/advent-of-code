{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2020.Day16
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInputAoC)
import AdventOfCode.Puzzle
import AdventOfCode.TH (defaultMainPuzzle)
import AdventOfCode.Util ((<.>))
import Control.Lens (makeLenses, view, views)
import Data.Foldable.Extra (productOn', sumOn')
import Data.Function.Pointless ((.:))
import Data.Ix (inRange)
import Data.List ((\\))
import Data.List.Infinite qualified as Infinite
import Data.List.NonEmpty qualified as NE
import Linear (V2 (..))
import Relude
import Text.Trifecta (Parser, anyChar, char, comma, eof, manyTill, natural, sepByNonEmpty, symbol)

data Rule
  = Rule
  { _field :: !String,
    _location :: !(V2 (Int, Int))
  }
  deriving (Eq, Generic, Show)

type Ticket = NonEmpty Int

data Document
  = Document
  { _rules :: !(NonEmpty Rule),
    _myTicket :: !Ticket,
    _nearbyTickets :: !(NonEmpty Ticket)
  }
  deriving (Eq, Generic, Show)

makeLenses ''Document

main :: IO ()
main = $(defaultMainPuzzle)

getInput :: IO Document
getInput = parseInputAoC 2020 16 do
  Document
    <$> manyTillNonEmpty ticketRule (symbol "your ticket:")
    <*> sepByNonEmpty posInt comma
    <*> ( symbol "nearby tickets:"
            *> manyTillNonEmpty (sepByNonEmpty posInt comma) eof
        )

partOne :: SimplePuzzle Document Int
partOne = sumOn' <$> views rules (sum .: invalidFields) <*> view nearbyTickets

partTwo :: SimplePuzzle Document Int
partTwo = do
  validTickets <-
    map NE.toList
      <.> NE.filter
      <$> views rules (not .: any . invalidField)
      <*> view nearbyTickets
  possibleFields <-
    views rules $ NE.toList >>> \theRules ->
      transpose validTickets <&> \fields -> do
        Rule label ranges <- theRules
        guard (all (flip any ranges . flip inRange) fields)
        pure label
  n <- views nearbyTickets (fromIntegral . length)
  views myTicket (NE.!!) <&> \readTicket ->
    productOn' (readTicket . fst)
      $ filter (isPrefixOf "departure" . snd)
      $ zip [0 ..]
      $ concat
      $ Infinite.iterate refine possibleFields
      Infinite.!! n

refine :: (Eq a) => [[a]] -> [[a]]
refine haystacks = flip (foldr go) needles <$> haystacks
  where
    needles = [needle | needle@[_] <- haystacks]
    go needle haystack = bool (haystack \\ needle) needle (haystack == needle)

ticketRule :: Parser Rule
ticketRule = Rule <$> label <*> intRanges
  where
    label = manyTill anyChar (symbol ":")
    intRanges = V2 <$> (intRange <* symbol "or") <*> intRange
    intRange = (,) <$> (posInt <* char '-') <*> posInt

invalidFields :: NonEmpty Rule -> Ticket -> [Int]
invalidFields = NE.filter . invalidField

invalidField :: NonEmpty Rule -> Int -> Bool
invalidField knownRules n = not (any (any (`inRange` n) . _location) knownRules)

posInt :: Parser Int
posInt = fromInteger <$> natural

manyTillNonEmpty :: (Alternative m) => m a -> m sep -> m (NonEmpty a)
manyTillNonEmpty p end = (:|) <$> p <*> manyTill p end
