{-# LANGUAGE LambdaCase #-}

module X.Data.AST.Arithmetic.Evaluate where

import Data.Maybe
import X.Data.Context
import X.Utils.LeftToRight

substituteVariables :: ArithmeticUnion -> Context -> ArithmeticUnion
substituteVariables val ctx =
    let subVar = mkSimplifier "substitute variables" $ \case
            (Variable s) -> fromMaybe (Variable s) (get s ctx)
            v -> v
     in runSimplifier subVar val

evaluateValue :: ArithmeticUnion -> Context -> ArithmeticUnion
evaluateValue v ctx = substituteVariables v ctx @> simplify
