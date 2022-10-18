module X.TestUtils.Value where

import qualified X.Data.LeftAssociativeInfixChain as LAIC
import X.Data.Value
import X.Data.Operator
import X.Utils.LeftToRight

ac :: Value -> [(AdditiveOperator, Value)] -> Value
ac = LAIC.fromHeadAndList ||@>|| AdditiveChain

mc :: Value -> [(MultiplicativeOperator, Value)] -> Value
mc = LAIC.fromHeadAndList ||@>|| MultiplicativeChain
