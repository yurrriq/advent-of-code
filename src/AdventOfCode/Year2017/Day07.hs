{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Year2017.Day07 where

import AdventOfCode.Input (parseInput, parseString)
import AdventOfCode.TH (defaultMainMaybe, inputFilePath)
import Control.Monad (void)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as Graph
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
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

type GraphTuple = (Graph, Vertex -> ((Text, Integer), Text, [Text]), Text -> Maybe Vertex)

main :: IO ()
main = $(defaultMainMaybe)

partOne :: (MonadFail m) => GraphTuple -> m Text
partOne (graph, nodeFromVertex, _vertexFromKey) =
  case Graph.topSort graph of
    top : _ -> let (_, name, _) = nodeFromVertex top in pure name
    [] -> fail "Empty graph!"

partTwo :: (MonadFail m) => GraphTuple -> m Text
partTwo = undefined

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
    pure ((name, weight), name, above)

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
