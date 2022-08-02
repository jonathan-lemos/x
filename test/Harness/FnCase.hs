module Harness.FnCase where
import Harness.TestDSLMonad
import Harness.With
import Test.Hspec
import X.Utils.LeftToRight

data FnCaseType b = FnCaseEq b | FnCaseSatisfies (b -> Bool)

data FnCase a b = FnCase {
    fncInput :: a,
    fncOutput :: FnCaseType b,
    fncTitle :: String
}

type FnCaseMonad a b c = TestDSLMonad (FnCase a b) c

instance WithTitle (FnCase a b) where
    withTitle fnc t = fnc { fncTitle = t }

shouldEvalTo :: (Show a, Show b, Eq b) => a -> b -> FnCaseMonad a b ()
shouldEvalTo a b = liftTdm $ FnCase a (FnCaseEq b) (show a <> " should evaluate to " <> show b)

shouldEvalAndSatisfy :: (Show a, Show b) => a -> (b -> Bool) -> FnCaseMonad a b ()
shouldEvalAndSatisfy a b = liftTdm $ FnCase a (FnCaseSatisfies b) (show a <> " should satisfy function")

fnCaseToTitleAndExpectation :: (Eq b, Show b) => (a -> b) -> FnCase a b -> (String, Expectation)
fnCaseToTitleAndExpectation f (FnCase input (FnCaseEq output) title) =
    (title, f input `shouldBe` output)
fnCaseToTitleAndExpectation f (FnCase input (FnCaseSatisfies g) title) =
    (title, f input `shouldSatisfy` g)

fnDesc :: (Eq b, Show b) => String -> (a -> b) -> FnCaseMonad a b () -> SpecWith ()
fnDesc title fn fnc =
    describe title $ do
        tdmItems fnc
            |@>| fnCaseToTitleAndExpectation fn
            |@>| uncurry it
            @> sequence_
