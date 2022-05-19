{-# LANGUAGE TupleSections #-}

module Types.Graph.Graph where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Foldable
import qualified Data.Map as DM
import Data.Maybe
import qualified Data.Set as DS

type AdjMatrix e v = DM.Map v (DM.Map v e)

newtype Graph e v = Graph
    { adjMatrix :: AdjMatrix e v
    }

_mapAdjMatrix :: (AdjMatrix e v -> AdjMatrix e v) -> Graph e v -> Graph e v
_mapAdjMatrix f = Graph . f . adjMatrix

emptyGraph :: Graph e v
emptyGraph = Graph DM.empty

-- | Inserts a vertex into the graph if it doesn't already exist.
putVertex :: (Ord v) => v -> Graph e v -> Graph e v
putVertex a = _mapAdjMatrix (DM.insertWith (const id) a DM.empty)

-- | Inserts/updates an edge with data `e` from `a` to `b` into the graph. If either node doesn't already exist, it is created.
putEdge :: (Ord v) => e -> v -> v -> Graph e v -> Graph e v
putEdge e a b = _mapAdjMatrix (DM.adjust (DM.insert b e) a) . putVertex a . putVertex b

-- | Inserts an edge and its reverse with the same data into the graph.
putBiEdge :: (Ord v) => e -> v -> v -> Graph e v -> Graph e v
putBiEdge e a b = putEdge e b a . putEdge e a b

-- | Returns `True` if the node is present in the graph.
hasVertex :: (Ord v) => v -> Graph e v -> Bool
hasVertex a = isJust . DM.lookup a . adjMatrix

-- | Returns the value of the edge from `a` to `b` in the graph if there is one.
getEdgeValue :: (Ord v) => v -> v -> Graph e v -> Maybe e
getEdgeValue a b = DM.lookup b <=< DM.lookup a . adjMatrix

-- | Returns `True` if there is an edge from `a` to `b` in the graph.
hasEdge :: (Ord v) => v -> v -> Graph e v -> Bool
hasEdge a b = isJust . getEdgeValue a b

{- | Performs a depth-first search, keeping track of state like `foldr`.
Returns a list of paths for which `isMatch` returns `True` from first node to last node. Each path will have at least one node.

This function does not have any cycle protection, meaning you can be presented with the same node twice. Use `dfsUnseen` to only consider the unseen nodes.

## Parameters
* `shouldExplore` - Given the current state, current node, and next node, optionally returns the new state to use with the next node.
                    If `Nothing` is returned, the node is not explored.
* `isMatch`       - Given a node, returns `True` if it's a match, and `False` if not. A match will add the path as a list of nodes from start to end to the result set.
* `initialState`  - The initial state to pass to `shouldExplore`.
* `startNode`     - The node to start exploring from. If this node doesn't exist, then no searching will occur, and only this initial node will be considered.
-}
dfs :: Ord v => (s -> e -> v -> v -> Maybe s) -> (v -> Bool) -> s -> v -> Graph e v -> [[v]]
dfs shouldExplore isMatch initialState startNode graph =
    let exploreLevel currentState currentNode neighbors =
            concatMap (currentNode :)
                . ($ graph)
                . uncurry (dfs shouldExplore isMatch)
                <$> catMaybes ((\(vertex, edge) -> (,vertex) <$> shouldExplore currentState edge currentNode vertex) <$> DM.assocs neighbors)
        exploration = maybe [] (exploreLevel initialState startNode) (DM.lookup startNode (adjMatrix graph))
     in (if isMatch startNode then ([startNode] :) else id) exploration

-- | Given a `chooseSuccessor` function, returns a `chooseSuccessor` function that always returns `Nothing` on a node that has already been explored.
unseen :: Ord v => (b -> e -> v -> v -> Maybe b) -> ((b, DS.Set v) -> e -> v -> v -> Maybe (b, DS.Set v))
unseen chooseSuccessor (currentState, seen) edgeData currentNode nextNode =
    if DS.member nextNode seen
        then Nothing
        else (,DS.insert currentNode seen) <$> chooseSuccessor currentState edgeData currentNode nextNode

-- | Like `dfs`, but does not present a node as a potential next node if it has already been explored.
dfsUnseen :: Ord v => (b -> e -> v -> v -> Maybe b) -> (v -> Bool) -> b -> v -> Graph e v -> [[v]]
dfsUnseen chooseSuccessor isMatch initialState startNode = dfs (unseen chooseSuccessor) isMatch (initialState, DS.singleton startNode) startNode
