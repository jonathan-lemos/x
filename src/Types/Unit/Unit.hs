module Types.Unit.Unit where

import Control.Applicative
import Data.List
import qualified Data.Map as DM
import Data.Number.CReal

data Unit
    = BaseUnit
        { baseUnitName :: String
        }
    | DerivedUnit
        { derivedUnitName :: String
        , quantity :: CReal
        , unitPowers :: [(Unit, CReal)]
        }
    deriving (Ord)

{- | Reduces a unit to its base units and powers.
For example, a Newton would become [("kg", 1), ("m", 1), ("s", -2)]
-}
reduceUnit :: Unit -> [(String, CReal)]
reduceUnit u =
    let _reduceUnitMap :: Unit -> DM.Map String CReal
        _reduceUnitMap (BaseUnit s) = DM.singleton s 1
        _reduceUnitMap (DerivedUnit _ q l) =
            foldr
                (DM.unionWith (+))
                DM.empty
                ((\(u, f) -> (* f) <$> _reduceUnitMap u) <$> l)
     in sort $ DM.assocs (_reduceUnitMap u)

instance Eq Unit where
    a == b = reduceUnit a == reduceUnit b

instance Show Unit where
    show (BaseUnit s) = s
    show (DerivedUnit s _ _) = s
