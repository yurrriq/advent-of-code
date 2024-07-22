{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Year2017.Day07 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (inputFilePath)
import Control.Monad (void)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as Graph
import Data.List (unfoldr)
import Data.List.Extra (maximumOn, sumOn')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Tuple.Extra (dupe, snd3, thd3)
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

type GraphTuple = (Graph, Vertex -> (Integer, Text, [Text]), Text -> Maybe Vertex)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    TextIO.putStrLn =<< partOne input
    putStr "Part Two: "
    print =<< partTwo input

partOne :: (MonadFail m) => GraphTuple -> m Text
partOne (graph, nodeFromVertex, _vertexFromKey) =
  case Graph.topSort graph of
    top : _ -> pure (snd3 (nodeFromVertex top))
    [] -> fail "Empty graph!"

partTwo :: (MonadFail m) => GraphTuple -> m Integer
partTwo (graph, nodeFromVertex, vertexFromKey) =
  case map nodeFromVertex . take 2 . reverse $ iterateMaybe go (head (Graph.topSort graph)) of
    [(weight, _, _), (_, _, stack)] ->
      let weightsAbove = map weightAbove stack
       in pure $ weight - maximum weightsAbove + minimum weightsAbove
    _unexpected -> fail "Shame!"
  where
    go vertex =
      let stack = thd3 (nodeFromVertex vertex)
          weights = [(k', weightAbove k') | k' <- stack]
          (maxK, maxWeight) = maximumOn snd weights
       in if all ((== maxWeight) . snd) weights
            then Nothing
            else vertexFromKey maxK
    weightAbove k =
      case nodeFromVertex <$> vertexFromKey k of
        Just (w, _, stack) -> w + sumOn' weightAbove stack
        Nothing -> 0

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : unfoldr (fmap dupe . f) x

getInput :: IO GraphTuple
getInput = parseInput parseGraph $(inputFilePath)

parseGraph :: Parser GraphTuple
parseGraph = fmap Graph.graphFromEdges . some $
  do
    name <- parseName <* space
    weight <- parens natural
    above <- fmap (fromMaybe []) . optional $
      do
        void (symbol "->")
        commaSep parseName <* newline
    pure (weight, name, above)

parseName :: Parser Text
parseName = Text.pack <$> some letter

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
