module Types.Unit.Context where

import Control.Applicative
import Data.Foldable
import Data.List
import qualified Data.Map as DM
import Data.Maybe
import Data.Number.CReal
import qualified Types.BiMap.BiMap as BM
import Types.Graph.Graph
import Types.Unit.ContextUnit
import Utils.Map
import Types.Unit.Exponential
import Types.Unit.BaseUnit

newtype UnitContext = UnitContext
    { unitGraph :: Graph CReal ContextUnit
    }

modifyGraph :: (Graph CReal ContextUnit -> Graph CReal ContextUnit) -> UnitContext -> UnitContext
modifyGraph f = UnitContext . f . unitGraph

emptyContext :: UnitContext
emptyContext = UnitContext emptyGraph

addUnit :: ContextUnit -> UnitContext -> UnitContext
addUnit u =
    case u of
        (ContextBaseUnit _) -> modifyGraph $ putVertex u
        (ContextDerivedUnit name quantity components) ->
            let addAllComponents = foldr (.) id $ addUnit . expBase <$> components
                addRelationships = modifyGraph $
                    case components of
                        [Exponential b 1] ->
                            putEdge (1 / quantity) u b
                            . putEdge quantity b u
                        _ -> id
            in addAllComponents . addRelationships

unitMultiply :: (CReal, ContextUnit) -> (CReal, ContextUnit) -> UnitContext -> (CReal, ContextUnit)
unitMultiply = undefined

unitDivide :: (CReal, ContextUnit) -> (CReal, ContextUnit) -> UnitContext -> (CReal, ContextUnit)
unitDivide = undefined
