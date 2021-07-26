-- https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md
module Data.Digraph.Lazy where

import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Prelude hiding (foldMap)

-- | A directed graph with labeled edges.
type Digraph vertex edgeLabel = Map vertex (Map vertex edgeLabel)

allAnscestors :: Ord v => Digraph v e -> Map v (Set v)
allAnscestors = allDescendants . transpose

allDescendants :: Ord v => Digraph v e -> Map v (Set v)
allDescendants = foldMap S.singleton (const id)

-- | Map each vertex into a monoid, transform with the edge label, and
-- combine the results with @('<>')@. This fold is right-asociative
-- and lazy in the accumulator.
foldMap ::
  (Ord v, Monoid m) => (v -> m) -> (e -> m -> m) -> Digraph v e -> Map v m
foldMap f g graph = graph'
  where
    graph' = M.foldMapWithKey go <$> graph
    go k v = f k <> F.foldMap (g v) (M.lookup k graph')

-- | Reverse all the edges of a directed graph, preserving the labels.
transpose :: Ord v => Digraph v e -> Digraph v e
transpose dag =
  M.fromListWith
    M.union
    [ (to, M.singleton from edgeLabel)
      | (from, vertices) <- M.toList dag,
        (to, edgeLabel) <- M.toList vertices
    ]
