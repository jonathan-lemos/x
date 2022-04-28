module Parser.Parsers.Combinator.Conditional where

import Parser.Parser
import Parser.Parsers.Combinator.Check

-- | If the parser succeeds, but the predicate returns `False`, fails with the default error message, otherwise passes the parser through.
conditional :: (a -> Bool) -> Parser a -> Parser a
conditional f = check f (const defaultErrMsg)
