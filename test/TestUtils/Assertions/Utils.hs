module TestUtils.Assertions.Utils where

import Test.Framework.TestInterface

flattenAssertions :: [Assertion] -> Assertion
flattenAssertions = sequence_
