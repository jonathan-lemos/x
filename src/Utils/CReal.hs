module Utils.CReal where

safeExp :: (Eq a, Floating a) => a -> a -> a
safeExp 0 _ = 0
safeExp a b = a ** b

absExp :: (Eq a, Floating a) => a -> a -> a
absExp a b = signum a * abs a `safeExp` b

safeDiv :: (Eq a, Fractional a) => a -> a -> a
safeDiv _ 0 = error "Cannot divide by 0"
safeDiv a b = a / b
