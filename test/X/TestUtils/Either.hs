module X.TestUtils.Either where

right :: (Show a) => Either a b -> b
right (Right x) = x
right (Left e) = error $ "Expected Right, got " <> show e

rightIs :: (b -> Bool) -> Either a b -> Bool
rightIs predicate (Right b) = predicate b
rightIs _predicate (Left _) = False

leftIs :: (a -> Bool) -> Either a b -> Bool
leftIs _predicate (Right _) = False
leftIs predicate (Left v) = predicate v
