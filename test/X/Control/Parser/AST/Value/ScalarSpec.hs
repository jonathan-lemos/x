module X.Control.Parser.AST.Value.ScalarSpec where

import X.Control.Parser.AST.Value.Scalar
import Test.Hspec
import X.Data.AST.Token.Scalar
import Harness.ParserCase
import X.Data.ParseError
import Harness.With

spec :: Spec
spec = do
   parserDesc scalar "scalar" $ do
        "2e4" `shouldParseTo` Number 2e4
        "-2.4" `shouldParseTo` Number (-2.4)
        "2" `shouldParseTo` Number 2
        "foo" `shouldParseTo` Variable "foo"
        "g" `shouldParseTo` Variable "g"

        "2e4 + 5" `shouldParseTo` Number 2e4 `withRemainder` " + 5"
        "foo * 7" `shouldParseTo` Variable "foo" `withRemainder` " * 7"

        "_" `shouldFailWithReason` "Expected a number or a variable name" `andRemainder` "_"
        "" `shouldFailWithReason` "Expected a number or a variable name" `andRemainder` ""
        " foo" `shouldFailWithReason` "Expected a number or a variable name" `andRemainder` " foo"
