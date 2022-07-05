{-# LANGUAGE ApplicativeDo #-}
module X.Control.Parser.AST.Token.IdentifierSpec where

import Test.Hspec
import X.Control.Parser.AST.Token.Identifier
import Control.Applicative
import Harness.ParserCase
import X.Data.ParseError

spec :: Spec
spec = parallel $ do
    parserDesc identifier "identifier" $ do
        "abc" `shouldTotallyParseTo` "abc"
        "d" `shouldTotallyParseTo` "d"

        "abc def" `shouldPartiallyParseTo` "abc" `withRemainder` " def"

        "_" `shouldFailWith` ParseError "Expected an A-z character" "_"
        "_abc" `shouldFailWith` ParseError "Expected an A-z character" "_abc"
        " abc" `shouldFailWith` ParseError "Expected an A-z character" " abc"
