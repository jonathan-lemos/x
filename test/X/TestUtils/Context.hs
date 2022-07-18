module X.TestUtils.Context where

import Data.Number.CReal
import X.Data.Value
import X.Data.Context

mkCtx :: [(String, Value)] -> Context
mkCtx = foldr (\(var, value) ctx -> put var value ctx) new
