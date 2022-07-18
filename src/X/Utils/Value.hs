module X.Utils.Value where

import X.Data.Value
import X.Data.Context
import X.Data.Operator
import Data.Maybe

isScalar :: Value -> Bool
isScalar (Scalar _) = True
isScalar _ = False

evaluateValue :: Value -> Context -> Value
evaluateValue (Scalar n) _ctx = Scalar n
evaluateValue (Variable s) ctx = fromMaybe (Variable s) (get s ctx)
evaluateValue (InfixCall a op b) ctx =
    case (evaluateValue a ctx, evaluateValue b ctx) of
        (Scalar sa, Scalar sb) -> Scalar $ applyOp op sa sb
        (na, nb) -> InfixCall na op nb
