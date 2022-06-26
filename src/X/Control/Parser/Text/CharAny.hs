module X.Control.Parser.Text.CharAny where

import Control.Applicative
import X.Control.Parser
import X.Control.Parser.Combinator.Atomic
import X.Control.Parser.Combinator.Branch.Check
import X.Control.Parser.Text.Char
import X.Utils.String (pluralize)

-- | Reads a character that matches any of the characters in the given string, failing on EOF or a character not in the string
charAny :: String -> Parser Char
charAny s =
    let errMsg = "Expected " <> pluralize (fmap show s)
     in atomic $ check
            (`elem` s)
            (const errMsg)
            (char <|> fail errMsg)
