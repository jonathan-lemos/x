module X.Data.ValueSpec where

import Harness.FnCase
import Test.Hspec
import X.Data.Value

spec :: Spec
spec = do
    fnDesc "xyxxx" id $ do
        Scalar 5 `shouldEvalTo` Scalar 5
--        Variable "foo" `shouldEvalTo` "foo"
--        ExpChain (Variable "foo") (Scalar 2) `shouldEvalTo` "foo^2"

