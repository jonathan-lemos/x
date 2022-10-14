module X.Control.Parser.Combinator.Possibly where

import X.Control.Parser
import Control.Applicative
import X.Utils.LeftToRight

-- | Runs the underlying parser, or if it fails, does nothing.
-- TODO: should return Maybe
possibly :: Parser a -> Parser (Maybe a)
possibly p = (p |@>| Just) <|> pure Nothing
