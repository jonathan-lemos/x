{-# LANGUAGE LambdaCase #-}

module X.Data.Value.Evaluate where

import Data.Maybe
import X.Data.Context
import X.Data.Value
import X.Data.Value.Simplifier
import X.Data.Value.Simplify
import X.Utils.LeftToRight

substituteVariables :: Value -> Context -> Value
substituteVariables val ctx =
    let subVar = mkSimplifier "substitute variables" $ \case
            (Variable s) -> fromMaybe (Variable s) (get s ctx)
            v -> v
     in runSimplifier subVar val

evaluateValue :: Value -> Context -> Value
evaluateValue v ctx = substituteVariables v ctx @> simplify
