module X.Control.Parser.Combinator.Branch.CheckSpec where

import Test.Hspec
import X.Control.Parser.Combinator.Branch.Check
import X.Control.Parser.AST.Token.Identifier
import Harness.With
import Harness.ParserCase
import X.Utils.LeftToRight

spec :: Spec
spec = do
    let sampleParser = check (length |@>| (< 5)) (<> " failed") identifier

    parserDesc sampleParser "check identifier with length < 5" $ do
        "abc" `shouldParseTo` "abc"

        "abc def" `shouldParseTo` "abc" `withRemainder` " def"

        "_" `shouldFailWithReason` "Expected an A-z character" `andRemainder` "_"
        "abcde" `shouldFailWithReason` "abcde failed" `andRemainder` ""
        "abcde ghi" `shouldFailWithReason` "abcde failed" `andRemainder` " ghi"
