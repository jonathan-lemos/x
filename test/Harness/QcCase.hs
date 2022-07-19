module Harness.QcCase where

import Data.Bifunctor
import Harness.TestCase
import Harness.TestDSLMonad
import Harness.With
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import X.Control.Parser
import X.Data.ParseError
import X.Utils.LeftToRight

data QcCase a = QcCase
    { qcTitle :: String
    , qcProperty :: a -> Expectation
    }

type QcCaseMonad a b = TestDSLMonad (QcCase a) b

instance WithTitle (QcCase a) where
    qc `withTitle` title = qc{qcTitle = title}

qcToSpec :: (Arbitrary a, Show a) => QcCase a -> Spec
qcToSpec (QcCase title fn) = prop title fn

qcDesc :: (Arbitrary a, Show a, Eq a) => String -> QcCaseMonad a b -> SpecWith ()
qcDesc title ps =
    describe title $ do
        tdmItems ps
            |@>| qcToSpec
            @> sequence_

shouldQcEq :: (Show a, Arbitrary a, Show b, Eq b) => (a -> b) -> (a -> b) -> QcCaseMonad a ()
shouldQcEq function expected =
    liftTdm $ QcCase "should quickcheck" (\x -> function x `shouldBe` expected x)
