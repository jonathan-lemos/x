module Types.Unit.Context where

import Control.Applicative
import Data.Foldable
import Data.List
import qualified Data.Map as DM
import Data.Number.CReal
import qualified Types.BiMap.BiMap as BM
import Types.Graph.Graph
import Types.Unit.Unit
import Utils.Map
import Data.Maybe

newtype UnitContext = UnitContext
    { unitGraph :: Graph (CReal, [(String, CReal)]) Unit
    }

_mapGraph :: (Graph (CReal, [(String, CReal)]) Unit -> Graph (CReal, [(String, CReal)]) Unit) -> UnitContext -> UnitContext
_mapGraph f = UnitContext . f . unitGraph

emptyContext :: UnitContext
emptyContext = UnitContext emptyGraph

addUnit :: Unit -> UnitContext -> UnitContext
addUnit u =
    _mapGraph $ putVertex u

unitMultiply :: (CReal, Unit) -> (CReal, Unit) -> UnitContext -> (CReal, Unit)
unitMultiply = undefined

unitDivide :: (CReal, Unit) -> (CReal, Unit) -> UnitContext -> (CReal, Unit)
unitDivide = undefined
