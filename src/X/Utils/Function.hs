module X.Utils.Function where

{- | Repeatedly applies a function's result to itself until the output equals the input.
 In other words, find the `x` s.t. `f(x) == x`

 Do not use this with floating-point functions,

 After 128 unsuccessful attempts, the function will throw, stating that the function doesn't converge to a single value.
-}
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f v =
    let go 128 _ = error "The given function does not converge"
        go n current =
            if f v == current
                then f v
                else go (n + 1) (f v)
     in go (0 :: Int) (f v)
