module Parser.Parsers.Combinator.Branch.Conditional where

import Parser.Parser
import Parser.Parsers.Combinator.Branch.Check

{- | If the parser succeeds, but the predicate returns `False`, fails with the default error message, otherwise passes the parser through.

 ## Examples

 >>> import Data.Char
 >>> import Parser.Parsers.Text.Char

 >>> parse (conditional isDigit char) "3 abc"
 Right (" abc",'3')

 >>> parse (conditional isDigit char) "a abc"
 Left (ParseError {reason = "Syntax Error", currentInput = " abc"})

 >>> parse (conditional isDigit char) ""
 Left (ParseError {reason = "Expected any character", currentInput = ""})
-}
conditional :: (a -> Bool) -> Parser a -> Parser a
conditional f = check f (const defaultErrMsg)
