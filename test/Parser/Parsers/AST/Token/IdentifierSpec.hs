{-# LANGUAGE TupleSections #-}
module Parser.Parsers.AST.Token.IdentifierSpec where

import Test.Hspec
import TestUtils.Parser
import Parser.Parsers.AST.Token.Identifier
import Control.Applicative

spec :: Spec
spec = do
    let asPassList = fmap $ liftA2 (,) id id
    let asFailList = fmap $ liftA2 (, "Expected an A-z character", ) id id

    passPartialFailSpec "identifier" identifier
        (asPassList ["abc", "d"])
        [("abc def", "abc", " def")]
        (asFailList ["_", "_abc", " abc"])
