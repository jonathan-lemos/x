module Harness.With where

class WithRemainder a where
    withRemainder :: a -> String -> a

class WithTitle a where
    withTitle :: a -> String -> a