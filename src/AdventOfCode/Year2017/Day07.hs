{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Year2017.Day07
  ( main,
    getInput,
    example,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (inputFilePath)
import AdventOfCode.Util (iterateMaybe)
import Control.Arrow ((&&&), (>>>))
import Control.Foldl qualified as Foldl
import Control.Monad (MonadPlus (mzero), ap, liftM2, (>=>))
import Data.Bifoldable (bisum)
import Data.Bifunctor (bimap)
import Data.Bitraversable (bisequence, bitraverse)
import Data.Function.Pointless ((.:))
import Data.Graph (Graph, Vertex)
import Data.Graph qualified as Graph
import Data.List qualified as List
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Tuple.Extra (fst3, snd3, thd3)
import Text.Trifecta
  ( Parser,
    commaSep,
    letter,
    natural,
    newline,
    optional,
    parens,
    some,
    space,
    symbol,
  )

type GraphTuple = (Graph, Vertex -> ProgramNode, ProgramName -> Maybe Vertex)

type ProgramNode = (Weight, ProgramName, [ProgramName])

type Weight = Integer

type ProgramName = Text

main :: IO ()
main = do
  input <- getInput
  putStr "Part One: "
  TextIO.putStrLn =<< partOne input
  putStr "Part Two: "
  print =<< partTwo input

getInput :: IO GraphTuple
getInput = parseInput parseGraph $(inputFilePath)

example :: IO GraphTuple
example =
  parseString parseGraph . unlines $
    [ "pbga (66)",
      "xhth (57)",
      "ebii (61)",
      "havc (66)",
      "ktlj (57)",
      "fwft (72) -> ktlj, cntj, xhth",
      "qoyq (66)",
      "padx (45) -> pbga, havc, qoyq",
      "tknk (41) -> ugml, padx, fwft",
      "jptl (61)",
      "ugml (68) -> gyxo, ebii, jptl",
      "gyxo (61)",
      "cntj (57)"
    ]

partOne :: (MonadFail m, MonadPlus m) => GraphTuple -> m ProgramName
partOne =
  dropThd3
    >>> bimap bottomVertex return
    >>> uncurry (flip ap)
    >>> fmap snd3

partTwo :: (MonadFail m, MonadPlus m) => GraphTuple -> m Weight
partTwo (graph, getProgram, programVertex) = do
  bottomVertex graph
    >>= (getProgram >>> subTower >>> last2 >=> excessWeight)
  where
    excessWeight =
      bitraverse
        (stack >>> mapM weightAbove >=> range)
        (return . programWeight)
        >=> (return . uncurry subtract)

    subTower =
      iterateMaybe $
        stack
          >>> uniqueMaximumOn weightAbove
          >=> lookupProgram

    weightAbove :: (MonadFail m) => ProgramName -> m Weight
    weightAbove =
      lookupProgram
        >=> sumOnM weightAbove . stack &&& return . programWeight
        >>> bisequence
        >>> fmap bisum

    lookupProgram :: (MonadFail m) => ProgramName -> m ProgramNode
    lookupProgram =
      programVertex
        >>> fmap getProgram
        >>> maybeFail "Unknown program!"

parseGraph :: Parser GraphTuple
parseGraph = fmap Graph.graphFromEdges . some $ do
  name <- parseName <* space
  weight <- parens natural
  above <- fmap (fromMaybe []) . optional $ do
    symbol "->" *> commaSep parseName <* newline
  pure (weight, name, above)

parseName :: Parser Text
parseName = Text.pack <$> some letter

bottomVertex :: (MonadFail m) => Graph -> m Vertex
bottomVertex = maybeFail "Empty graph!" . listToMaybe . Graph.topSort

programWeight :: ProgramNode -> Weight
programWeight = fst3

stack :: ProgramNode -> [Text]
stack = thd3

-- | The last two elements of a list: @(penultimate, ultimate)@.
last2 :: (MonadPlus m) => [a] -> m (a, a)
last2 = uncurry (liftA2 (,)) . List.foldl' (curry (bimap snd return)) (mzero, mzero)
{-# INLINE last2 #-}

-- | The difference between the maximum and minimum elements.
range :: (Foldable t, Ord a, Num a, MonadFail m) => t a -> m a
range =
  maybeFail "Empty structure!" .: Foldl.fold $ do
    liftM2 (-) <$> Foldl.maximum <*> Foldl.minimum

-- | Compute the sum of all elements using a given monadic valuation function.
sumOnM :: (Foldable f, Monad m, Num b) => (a -> m b) -> f a -> m b
sumOnM = Foldl.foldM . flip Foldl.premapM (Foldl.generalize Foldl.sum)

-- | The unique maximum element with respect to the given comparison function.
uniqueMaximumOn :: (Ord b) => (a -> Maybe b) -> [a] -> Maybe a
uniqueMaximumOn f = Foldl.fold $ Foldl.Fold step (Nothing, True) finish
  where
    step (Nothing, _) y = (Just y, True)
    step (Just x, unique) y =
      case comparing f y x of
        GT -> (Just y, True)
        EQ -> (Just x, False)
        LT -> (Just x, unique)
    finish (Just x, True) = Just x
    finish _ = Nothing

-- | Lift a 'Maybe' to 'MonadFail' with a given failure reason.
maybeFail :: (MonadFail m) => String -> Maybe a -> m a
maybeFail reason = maybe (fail reason) return

dropThd3 :: (a, b, c) -> (a, b)
dropThd3 (x, y, _) = (x, y)
