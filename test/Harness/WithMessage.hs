module Harness.WithMessage where

class WithMessage a where
    withMessage :: a -> String -> a