module Parser.Parsers.Combinator.Possibly where

import Parser.Parser
import Control.Monad
import Control.Applicative

possibly :: Parser a -> Parser ()
possibly p = void p <|> mempty
