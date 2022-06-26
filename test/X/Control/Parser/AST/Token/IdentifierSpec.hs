{-# LANGUAGE TupleSections #-}
module X.Control.Parser.AST.Token.IdentifierSpec where

import Test.Hspec
import X.TestUtils.Parser
import X.Control.Parser.AST.Token.Identifier
import Control.Applicative

spec :: Spec
spec = parallel $ do
    let asPassList = fmap $ liftA2 (,) id id
    let asFailList = fmap $ liftA2 (, "Expected an A-z character", ) id id

    passPartialFailSpec "identifier" identifier
        (asPassList ["abc", "d"])
        [("abc def", "abc", " def")]
        (asFailList ["_", "_abc", " abc"])
