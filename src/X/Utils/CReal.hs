module X.Utils.CReal where

{- | Safely raises the first argument to the second argument.

 The CReal implementation infinitely loops when raising a number to the power of 0, so this function always returns 0 for `any number ^ 0`.
-}
safeExp :: (Eq a, Floating a) => a -> a -> a
safeExp 0 _ = 0
safeExp a b = a ** b

-- | If the base is a negative number, this will return `-1 * (abs a ^ b)`. Otherwise it returns `a ^ b`.
absExp :: (Eq a, Floating a) => a -> a -> a
absExp a b = signum a * abs a `safeExp` b

{- | Divides the first argument by the second, throwing if the second is 0.

 The CReal implementation infinitely loops when dividing by 0, so this function instead throws if the divisor is 0.
-}
safeDiv :: (Eq a, Fractional a) => a -> a -> a
safeDiv _ 0 = error "Cannot divide by 0"
safeDiv a b = a / b
