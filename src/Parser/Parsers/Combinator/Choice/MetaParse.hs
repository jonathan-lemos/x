module Parser.Parsers.Combinator.Choice.MetaParse where

import Control.Monad
import Parser.Parser
import Parser.Parsers.Combinator.MapResult

{- | Given a parser that chooses a parser (metaparser), outputs the underlying parser or the error.
If the metaparser succeeds, the underlying parser starts where the metaparser *started*. This is how `metaParse` differs from `join`, which continues at the *end* of the first parser.
-}
metaParse :: Parser (Parser a) -> Parser a
metaParse = mapResultWithInput $ \input e ->
    e >>= \(_newInput, parser) -> parse parser input
