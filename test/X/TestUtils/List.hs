module X.TestUtils.List where

weave :: [a] -> [a] -> [a]
weave a b =
    let weaveL (a:as) bs = a:weaveR as bs
        weaveL [] bs = bs
        weaveR as (b:bs) = b:weaveL as bs
        weaveR as [] = as
        in weaveL a b

