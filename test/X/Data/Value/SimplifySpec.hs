module X.Data.Value.SimplifySpec where

import Harness.FnCase
import Harness.TestCase
import Harness.With
import Test.Hspec
import X.Data.Operator
import X.Data.Value
import X.Data.Value.Simplify
import X.Utils.LeftToRight

fnShowDesc :: (Show a, Eq b, Show b) => String -> (a -> b) -> FnCaseMonad a String () -> SpecWith ()
fnShowDesc title f = fnDesc title (f |@>| show)

shouldNotChangeDesc :: (Show a, Eq a) => String -> (a -> a) -> [a] -> SpecWith ()
shouldNotChangeDesc title f as =
    fnDesc title f $ (as |@>| \a -> a `shouldEvalTo` a `withTitle` (show a <> " should not change")) @> sequence_

spec :: Spec
spec = do
    shouldNotChangeDesc
        "simplifySingleElementChain should not touch these"
        simplifySingleElementChain
        [ Scalar 1
        , Variable "foo"
        , ExpChain (Scalar 1) (Scalar 1)
        , AdditiveChain (Scalar 1) [(Add, Scalar 2)]
        , MultiplicativeChain (Scalar 1) [(Mul, Scalar 2)]
        ]

    fnDesc "simplifySingleElementChain should reduce single element chains" simplifySingleElementChain $ do
        AdditiveChain (Scalar 1) [] `shouldEvalTo` Scalar 1
        MultiplicativeChain (Scalar 1) [] `shouldEvalTo` Scalar 1

    shouldNotChangeDesc
        "simplifyExponentiationBy0Or1"
        simplifyExponentiationBy0Or1
        [ Scalar 1
        , Variable "foo"
        , ExpChain (Variable "foo") (Scalar 2)
        , AdditiveChain (Scalar 1) [(Add, Scalar 2)]
        , MultiplicativeChain (Scalar 1) [(Mul, Scalar 2)]
        ]

    fnDesc "simplifyExponentiationBy0Or1" simplifyExponentiationBy0Or1 $ do
        ExpChain (Variable "foo") (Scalar 1) `shouldEvalTo` Variable "foo"
        ExpChain (Variable "foo") (Scalar 0) `shouldEvalTo` Scalar 1
        ExpChain (Scalar 5) (Scalar 1) `shouldEvalTo` Scalar 5

    shouldNotChangeDesc
        "simplifyMultiplyBy0"
        simplifyMultiplyBy0
        [ Scalar 1
        , Variable "foo"
        , ExpChain (Scalar 1) (Scalar 2)
        , AdditiveChain (Scalar 1) [(Add, Scalar 2)]
        , MultiplicativeChain (Scalar 1) [(Mul, Scalar 2)]
        ]

    fnDesc "simplifyMultiplyBy0" simplifyMultiplyBy0 $ do
        MultiplicativeChain (Scalar 0) [] `shouldEvalTo` Scalar 0
        MultiplicativeChain (Scalar 0) [(Mul, Variable "foo")] `shouldEvalTo` Scalar 0
        MultiplicativeChain (Variable "foo") [(Mul, Scalar 0)] `shouldEvalTo` Scalar 0

    shouldNotChangeDesc
        "simplifyAdd0"
        simplifyAdd0
        [ Scalar 1
        , Variable "foo"
        , AdditiveChain (Scalar 1) []
        , AdditiveChain (Scalar 1) [(Add, Scalar 2)]
        ]

    fnDesc "simplifyAdd0" simplifyAdd0 $ do
        AdditiveChain (Scalar 0) [] `shouldEvalTo` Scalar 0
        AdditiveChain (Scalar 0) [(Add, Scalar 5)] `shouldEvalTo` AdditiveChain (Scalar 5) []
        AdditiveChain (Scalar 0) [(Add, Scalar 5), (Sub, Variable "x")] `shouldEvalTo` AdditiveChain (Scalar 5) [(Sub, Variable "x")]

    shouldNotChangeDesc
        "simplifyMultiply1"
        simplifyMultiply1
        [ Scalar 1
        , Variable "foo"
        , MultiplicativeChain (Scalar 1) []
        , MultiplicativeChain (Scalar 1) [(Mul, Scalar 2)]
        ]

    fnDesc "simplifyMultiply1" simplifyMultiply1 $ do
        MultiplicativeChain (Scalar 1) [] `shouldEvalTo` Scalar 1
        MultiplicativeChain (Scalar 1) [(Mul, Scalar 5)] `shouldEvalTo` MultiplicativeChain (Scalar 5) []
        MultiplicativeChain (Scalar 1) [(Mul, Scalar 5), (Div, Variable "x")] `shouldEvalTo` MultiplicativeChain (Scalar 5) [(Div, Variable "x")]

    shouldNotChangeDesc
        "simplifyMultiplyingByReciprocal"
        simplifyMultiplyingByReciprocal
        [ Scalar 1
        , Variable "foo"
        , MultiplicativeChain (Scalar 1) []
        , MultiplicativeChain (Scalar 1) [(Div, Scalar 2)]
        ]

    fnDesc "simplifyMultiplyingByReciprocal" simplifyMultiplyingByReciprocal $ do
        MultiplicativeChain (Scalar 3) [(Mul, Scalar 0.5)] `shouldEvalTo` MultiplicativeChain (Scalar 3) [(Div, Scalar 2)]
        MultiplicativeChain (Scalar 0.5) [(Mul, Scalar 3)] `shouldEvalTo` MultiplicativeChain (Scalar 3) [(Div, Scalar 2)]
