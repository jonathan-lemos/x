module X.Control.Parser.Text.Literal where

import Control.Applicative
import X.Control.Parser
import X.Control.Parser.Text.CharEq

-- | Matches the given string only
literal :: String -> Parser String
literal s =
    let parsers = (\c -> (:[]) <$> charEq c <|> fail ("Expected " <> show s)) <$> s
    in mconcat parsers
