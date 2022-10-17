module X.Control.Parser.Combinator.Possibly where

import X.Control.Parser
import Control.Monad
import Control.Applicative

-- | Runs the underlying parser, or if it fails, does nothing.
-- TODO: should return Maybe
possibly :: Parser a -> Parser ()
possibly p = void p <|> mempty
