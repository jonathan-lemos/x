module X.Utils.Function where

-- Pipe operator. Sends the output of the first function into the input of the second.
(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) = flip (.)
infixl 4 |>

-- Applies the given argument to the given function. Flipped version of ($)
(>$) :: a -> (a -> b) -> b
(>$) = flip ($)
infixl 4 >$
