module X.Evaluation.ToValue where

import X.Data.State.Value
import X.Data.State.XState

-- | Can be converted to a value, possibly with an error message instead.
class ToValue a where
    toValue :: a -> XState -> Either String Value

instance ToValue Value where
    toValue = const . Right
