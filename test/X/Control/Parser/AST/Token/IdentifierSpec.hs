{-# LANGUAGE ApplicativeDo #-}
module X.Control.Parser.AST.Token.IdentifierSpec where

import Test.Hspec
import X.Control.Parser.AST.Token.Identifier
import Control.Applicative
import Harness.ParserCase
import X.Data.ParseError
import Harness.With

spec :: Spec
spec = parallel $ do
    parserDesc identifier "identifier" $ do
        "abc" `shouldParseTo` "abc"
        "d" `shouldParseTo` "d"

        "abc def" `shouldParseTo` "abc" `withRemainder` " def"

        "_" `shouldFailWithReason` "Expected an A-z character" `andRemainder` "_"
        "_abc" `shouldFailWithReason` "Expected an A-z character" `andRemainder` "_abc"
        " abc" `shouldFailWithReason` "Expected an A-z character" `andRemainder` " abc"
