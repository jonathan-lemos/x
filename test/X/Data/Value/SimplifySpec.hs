module X.Data.Value.SimplifySpec where

import Harness.FnCase
import Harness.TestCase
import Harness.With
import Test.Hspec
import X.Control.Parser
import X.Control.Parser.AST.ArithmeticExpression
import X.Data.Operator
import X.Data.Value
import X.Data.Value.Simplify
import X.TestUtils.Either
import X.Utils.LeftToRight

parseValue :: String -> Value
parseValue =
    parse additiveExpression
        |@>| right
        |@>| snd

simplifyDesc :: String -> (Value -> Value) -> FnCaseMonad String String () -> SpecWith ()
simplifyDesc title f m =
    let modifiedMapper :: String -> String
        modifiedMapper = parseValue |@>| f |@>| show
     in fnDesc title modifiedMapper m

fnShowDesc :: (Show a, Eq b, Show b) => String -> (a -> b) -> FnCaseMonad a String () -> SpecWith ()
fnShowDesc title f = fnDesc title (f |@>| show)

shouldNotChangeDesc :: String -> (Value -> Value) -> [String] -> SpecWith ()
shouldNotChangeDesc title f as =
    fnDesc title f $ (as |@>| \a -> parseValue a `shouldEvalTo` parseValue a `withTitle` (show a <> " should not change")) @> sequence_

spec :: Spec
spec = do
    shouldNotChangeDesc
        "simplifySingleElementChain should not touch these"
        simplifySingleElementChain
        [ "1"
        , "foo"
        , "1^2"
        , "1+2"
        , "1*2"
        ]

    fnDesc "simplifySingleElementChain should reduce single element chains" simplifySingleElementChain $ do
        AdditiveChain (Scalar 1) [] `shouldEvalTo` Scalar 1
        MultiplicativeChain (Scalar 1) [] `shouldEvalTo` Scalar 1

    shouldNotChangeDesc
        "simplifyExponentiationBy0Or1"
        simplifyExponentiationBy0Or1
        [ "1"
        , "foo"
        , "foo^2"
        , "foo+2"
        , "foo*2"
        ]

    simplifyDesc "simplifyExponentiationBy0Or1" simplifyExponentiationBy0Or1 $ do
        "foo^1" `shouldEvalTo` "foo"
        "foo^0" `shouldEvalTo` "1"
        "5^1" `shouldEvalTo` "5"

    shouldNotChangeDesc
        "simplifyMultiplyBy0"
        simplifyMultiplyBy0
        [ "1"
        , "foo"
        , "1^2"
        , "1+2"
        , "1*2"
        ]

    fnDesc "simplifyMultiplyBy0" simplifyMultiplyBy0 $ do
        MultiplicativeChain (Scalar 0) [] `shouldEvalTo` Scalar 0
        MultiplicativeChain (Scalar 0) [(Mul, Variable "foo")] `shouldEvalTo` Scalar 0
        MultiplicativeChain (Variable "foo") [(Mul, Scalar 0)] `shouldEvalTo` Scalar 0

    shouldNotChangeDesc
        "simplifyAdd0"
        simplifyAdd0
        [ "1"
        , "foo"
        , "1^2"
        , "1*2"
        , "1+2"
        ]

    fnDesc "simplifyAdd0" simplifyAdd0 $ do
        AdditiveChain (Scalar 0) [] `shouldEvalTo` Scalar 0
        AdditiveChain (Scalar 0) [(Add, Scalar 5)] `shouldEvalTo` AdditiveChain (Scalar 5) []
        AdditiveChain (Scalar 0) [(Add, Scalar 5), (Sub, Variable "x")] `shouldEvalTo` AdditiveChain (Scalar 5) [(Sub, Variable "x")]

    shouldNotChangeDesc
        "simplifyMultiply1"
        simplifyMultiply1
        [ "1"
        , "foo"
        , "2*3"
        , "2+3"
        , "2^3"
        ]

    simplifyDesc "simplifyMultiply1" simplifyMultiply1 $ do
        "1*5" `shouldEvalTo` "5"
        "5*1" `shouldEvalTo` "5"
        "1*1*5*1" `shouldEvalTo` "5"
        "1*5x" `shouldEvalTo` "5x"

    shouldNotChangeDesc
        "simplifyMultiplyingByReciprocal"
        simplifyMultiplyingByReciprocal
        [ "1"
        , "foo"
        , "2*3"
        , "2+3"
        , "2^3"
        ]

    simplifyDesc "simplifyMultiplyingByReciprocal" simplifyMultiplyingByReciprocal $ do
        "3*0.5" `shouldEvalTo` "3/2"
        "0.5*3" `shouldEvalTo` "3/2"

    shouldNotChangeDesc
        "simplifySortChainTerms"
        simplifySortChainTerms
        [ "1"
        , "foo"
        , "3*foo"
        , "foo+3"
        ]

    simplifyDesc "simplifySortChainTerms" simplifySortChainTerms $ do
        "foo*3" `shouldEvalTo` "3*foo"
        "3+foo" `shouldEvalTo` "foo+3"

    shouldNotChangeDesc
        "simplifyLikeAdditiveTerms"
        simplifyLikeAdditiveTerms
        [ "1"
        , "foo"
        , "3+2"
        , "3*2"
        , "3^2"
        ]

    simplifyDesc "simplifyLikeAdditiveTerms" simplifyLikeAdditiveTerms $ do
        "3+2" `shouldEvalTo` "5"
        "3-2" `shouldEvalTo` "1"
        "3*x+2*x" `shouldEvalTo` "5*x"
        "3*x-2*x" `shouldEvalTo` "1*x"
        "3+2+3*x+2*x" `shouldEvalTo` "5+5*x"

    shouldNotChangeDesc
        "simplifyLikeMultiplicativeTerms"
        simplifyLikeAdditiveTerms
        [ "1"
        , "foo"
        , "3+2"
        , "3*2"
        , "3^2"
        ]

    simplifyDesc "simplifyLikeMultiplicativeTerms" simplifyLikeMultiplicativeTerms $ do
        "3*2" `shouldEvalTo` "6"
        "6/2" `shouldEvalTo` "3"
        "3*x*2" `shouldEvalTo` "6*x"
        "3*x*2*y" `shouldEvalTo` "6*x*y"


    shouldNotChangeDesc
        "simplifyLikeExpTerms"
        simplifyLikeExpTerms
        [ "1"
        , "foo"
        , "3+2"
        , "3*2"
        ]

    simplifyDesc "simplifyLikeExpTerms" simplifyLikeExpTerms $ do
        "3^2" `shouldEvalTo` "9"
        "3*x*x" `shouldEvalTo` "3*x^2"
