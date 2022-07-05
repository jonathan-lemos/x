module X.Control.Parser.AST.Value.ScalarSpec where

import X.Control.Parser.AST.Value.Scalar
import Test.Hspec
import X.Data.AST.Token.Scalar
import Harness.ParserCase
import X.Data.ParseError

spec :: Spec
spec = do
   parserDesc scalar "scalar" $ do
        "2e4" `shouldTotallyParseTo` Number 2e4
        "-2.4" `shouldTotallyParseTo` Number (-2.4)
        "2" `shouldTotallyParseTo` Number 2
        "foo" `shouldTotallyParseTo` Variable "foo"
        "g" `shouldTotallyParseTo` Variable "g"

        "2e4 + 5" `shouldPartiallyParseTo` Number 2e4 `withRemainder` " + 5"
        "foo * 7" `shouldPartiallyParseTo` Variable "foo" `withRemainder` " * 7"

        "_" `shouldFailWith` ParseError "Expected a number or a variable name" "_"
        "" `shouldFailWith` ParseError "Expected a number or a variable name" ""
        " foo" `shouldFailWith` ParseError "Expected a number or a variable name" " foo"
