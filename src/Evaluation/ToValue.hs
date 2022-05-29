module Evaluation.ToValue where

import State.Value
import State.XState

class ToValue a where
    toValue :: a -> XState -> Either String Value
