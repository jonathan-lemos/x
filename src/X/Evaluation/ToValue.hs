module X.Evaluation.ToValue where

import X.Data.State.Value
import X.Data.State.XState
import X.Control.Try

-- | Can be converted to a value, possibly with an error message instead.
class ToValue a where
    toValue :: a -> XState -> Try Value

instance ToValue Value where
    toValue = const . Success
