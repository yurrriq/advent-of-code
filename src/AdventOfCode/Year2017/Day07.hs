{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2017.Day07
  ( main,
    getInput,
    getExample,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInputAoC, parseString)
import AdventOfCode.SimplePuzzle
import AdventOfCode.TH (evalPuzzle)
import AdventOfCode.Util (iterateMaybe, maybeFail, (<.>))
import Control.Foldl qualified as Foldl
import Control.Monad (liftM2)
import Data.Bifoldable (bisum)
import Data.Function.Pointless ((.:))
import Data.Graph (Graph, Vertex)
import Data.Graph qualified as Graph
import Data.Text qualified as Text
import Data.Tuple.Extra (fst3, snd3, thd3)
import Relude
import Text.Trifecta
  ( Parser,
    commaSep,
    letter,
    natural,
    newline,
    parens,
    space,
    symbol,
  )

type GraphTuple = (Graph, Vertex -> ProgramNode, ProgramName -> Maybe Vertex)

type Weight = Integer

type ProgramName = Text

type ProgramNode = (Weight, ProgramName, [ProgramName])

main :: IO ()
main = $(evalPuzzle)

getInput :: IO GraphTuple
getInput = parseInputAoC 2017 7 parseGraph

getExample :: IO GraphTuple
getExample = parseString parseGraph example

example :: String
example =
  "pbga (66)\n\
  \xhth (57)\n\
  \ebii (61)\n\
  \havc (66)\n\
  \ktlj (57)\n\
  \fwft (72) -> ktlj, cntj, xhth\n\
  \qoyq (66)\n\
  \padx (45) -> pbga, havc, qoyq\n\
  \tknk (41) -> ugml, padx, fwft\n\
  \jptl (61)\n\
  \ugml (68) -> gyxo, ebii, jptl\n\
  \gyxo (61)\n\
  \cntj (57)\n"

partOne :: SimplePuzzle GraphTuple ProgramName
partOne = do
  (graph, getProgram, _) <- ask
  programName . getProgram <$> bottomVertex graph

partTwo :: SimplePuzzle GraphTuple Weight
partTwo =
  ask >>= \(graph, getProgram, programVertex) ->
    let lookupProgram = getProgram <.> programVertex

        weightAbove =
          lookupProgram
            >=> sumOnM weightAbove
            . stack
            &&& programWeight
            >>> bisequence
            >>> fmap bisum

        subTower =
          iterateMaybe
            $ stack
            >>> uniqueMaximumOn weightAbove
            >=> lookupProgram

        excessWeight =
          uncurry subtract
            <.> bitraverse
              (stack >>> mapM weightAbove >=> range)
              programWeight
     in maybeFail "cannot determine program with wrong or its correct weight"
          $ bottomVertex graph
          <&> subTower
          . getProgram
          >>= last2
          >>= excessWeight

parseGraph :: Parser GraphTuple
parseGraph = fmap Graph.graphFromEdges . some $ do
  name <- parseName <* space
  weight <- parens natural
  above <- fmap (fromMaybe []) . optional $ do
    symbol "->" *> commaSep parseName <* newline
  pure (weight, name, above)

parseName :: Parser ProgramName
parseName = Text.pack <$> some letter

bottomVertex :: (MonadFail m) => Graph -> m Vertex
bottomVertex = maybeFail "Empty graph!" . listToMaybe . Graph.topSort

programName :: ProgramNode -> ProgramName
programName = snd3

programWeight :: (MonadFail m) => ProgramNode -> m Weight
programWeight = return . fst3

stack :: ProgramNode -> [ProgramName]
stack = thd3

-- | The last two elements of a list: @(penultimate, ultimate)@.
last2 :: (MonadFail m) => [a] -> m (a, a)
last2 =
  maybeFail "ope!"
    . bisequence
    . foldl' (curry (bimap snd return)) (Nothing, Nothing)
{-# INLINE last2 #-}

-- | The difference between the maximum and minimum elements.
range :: (Foldable t, Ord a, Num a, MonadFail m) => t a -> m a
range =
  maybeFail "Empty structure!"
    .: Foldl.fold $ do
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
