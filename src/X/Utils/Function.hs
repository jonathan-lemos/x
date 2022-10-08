module X.Utils.Function where

{- | Repeatedly applies a function's result to itself until the output equals the input.
 In other words, find the `x` s.t. `f(x) == x`

 Do not use this with floating-point functions, as floating-point imprecision will cause the function to not converge to a single value.

 After 64 unsuccessful attempts, the function will throw, stating that the function doesn't converge to a single value.
-}
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f v =
    let go 16 _ = error "The given function did not converge to a single value after 16 attempts"
        go n current =
            if f current == current
                then f current
                else go (n + 1) (f current)
     in go (0 :: Int) (f v)
