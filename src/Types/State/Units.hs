module Types.State.Units where

import Types.State.XState
import Types.Unit.Unit
import qualified Data.Map as DM
import Data.Char

putUnit :: Unit -> XState -> XState
putUnit u =
    let key = case u of
                BaseUnit {baseUnitName = s} -> s
                DerivedUnit {derivedUnitName = s} -> s
    in mapUnits $ DM.insert (toLower <$> key) u

getUnit :: String -> XState -> Maybe Unit
getUnit s = DM.lookup (toLower <$> s) . units
