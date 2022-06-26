module X.TestUtils.Either where

right :: Either String b -> b
right (Right x) = x
right (Left e) = error e
