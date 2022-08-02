module X.Data.Value.Evaluate where

import X.Data.Value
import X.Data.Context
import Data.Maybe
import X.Data.Value.Simplify
import X.Utils.LeftToRight

substituteVariables :: Value -> Context -> Value
substituteVariables val ctx =
    let subVar v =
            case v of
                (Variable s) -> fromMaybe (Variable s) (get s ctx)
                v -> v
    in deepSimplify subVar val

evaluateValue :: Value -> Context -> Value
evaluateValue v ctx = substituteVariables v ctx @> simplify
