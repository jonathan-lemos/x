module X.Control.Parser.Combinator.Branch.Conditional where

import X.Control.Parser
import X.Control.Parser.Combinator.Branch.Check

-- | If the parser succeeds, but the predicate returns `False`, fails with the default error message, otherwise passes the parser's result through.
conditional :: (a -> Bool) -> Parser a -> Parser a
conditional f = check f (const defaultErrMsg)
