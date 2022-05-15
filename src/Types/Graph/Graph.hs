{-# LANGUAGE TupleSections #-}

module Types.Graph.Graph where

import Control.Applicative
import Data.Bifunctor
import qualified Data.Map as DM
import Data.Maybe
import qualified Data.Set as DS

newtype Graph a = Graph
    { adjMatrix :: DM.Map a [a]
    }

instance Functor Graph where
    fmap f = Graph . DM.mapKeys f . (fmap . fmap) f . adjMatrix

_mapAdjMatrix :: (DM.Map a [a] -> DM.Map a [a]) -> Graph a -> Graph a
_mapAdjMatrix f = Graph . f . adjMatrix

-- | Inserts a node into the graph if it doesn't already exist.
putNode :: (Ord a) => a -> Graph a -> Graph a
putNode a = _mapAdjMatrix (DM.insertWith (const id) a [])

-- | Inserts an edge into the graph. If either node doesn't already exist, it is created.
putEdge :: (Ord a) => a -> a -> Graph a -> Graph a
putEdge a b = _mapAdjMatrix (DM.adjust (b :) a) . putNode a . putNode b

-- | Returns `True` if the node is present in the graph.
has :: (Ord a) => a -> Graph a -> Bool
has a = isJust . DM.lookup a . adjMatrix

-- | Returns `True` if there is an edge from `a` to `b` in the graph.
hasEdge :: (Ord a) => a -> a -> Graph a -> Bool
hasEdge a b graph =
    isJust $
        DM.lookup a (adjMatrix graph)
            >>= \l -> if b `elem` l then Just () else Nothing

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
dfs :: Ord a => (b -> a -> a -> Maybe b) -> (a -> Bool) -> b -> a -> Graph a -> [[a]]
dfs shouldExplore isMatch initialState startNode graph =
    let exploreLevel currentState currentNode neighbors =
            fmap (currentNode :)
                . ($ graph)
                . uncurry (dfs shouldExplore isMatch)
                <$> catMaybes (liftA2 fmap (flip (,)) (shouldExplore currentState currentNode) <$> neighbors)
        exploration = case DM.lookup startNode (adjMatrix graph) of
            Just neighbors -> concat $ exploreLevel initialState startNode neighbors
            Nothing -> []
     in (if isMatch startNode then ([startNode] :) else id) exploration

-- | Given a `chooseSuccessor` function, returns a `chooseSuccessor` function that always returns `Nothing` on a node that has already been explored.
unseen :: Ord a => (b -> a -> a -> Maybe b) -> ((b, DS.Set a) -> a -> a -> Maybe (b, DS.Set a))
unseen chooseSuccessor (currentState, seen) currentNode nextNode =
    if DS.member nextNode seen
        then Nothing
        else (,DS.insert currentNode seen) <$> chooseSuccessor currentState currentNode nextNode

-- | Like `dfs`, but does not present a node as a potential next node if it has already been explored.
dfsUnseen :: Ord a => (b -> a -> a -> Maybe b) -> (a -> Bool) -> b -> a -> Graph a -> [[a]]
dfsUnseen chooseSuccessor isMatch initialState startNode = dfs (unseen chooseSuccessor) isMatch (initialState, DS.singleton startNode) startNode
