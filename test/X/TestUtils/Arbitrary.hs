{-# OPTIONS_GHC -Wno-orphans #-}
module X.TestUtils.Arbitrary where

import Test.QuickCheck
import Data.Number.CReal

instance Arbitrary CReal where
    arbitrary = read . show <$> (arbitrary :: Gen Double)
