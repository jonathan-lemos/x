module Utils.Either where

eitherFromMaybe :: a -> Maybe b -> Either a b
eitherFromMaybe left Nothing = Left left
eitherFromMaybe _ (Just right) = Right right

combineErrors :: Either String a -> Either String b -> Either String (a, b)
combineErrors a b =
    case (a, b) of
        (Left e1, Left e2) -> Left $ e1 <> "\n" <> e2
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right v1, Right v2) -> Right (v1, v2)
