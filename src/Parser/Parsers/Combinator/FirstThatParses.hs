module Parser.Parsers.Combinator.FirstThatParses where

import Parser.Error
import Parser.Parser

{- | Returns the result of the first parser that succeeds or parses at least one character (doesn't have to succeed).
Returns the given error message if none of them parse.

## Examples

>>> import Parser.Parsers.Text.Literal

>>> parse (firstThatParses [literal "abc", literal "def"] "errmsg") "abczzz"
Right ("zzz","abc")

>>> parse (firstThatParses [literal "abc", literal "def"] "errmsg") "abdzzz"
Left (ParseError {reason = "Expected \"abc\"", currentInput = "dzzz"})

>>> parse (firstThatParses [literal "abc", literal "def"] "errmsg") "defzzz"
Right ("zzz","def")

>>> parse (firstThatParses [literal "abc", literal "def"] "errmsg") "degzzz"
Left (ParseError {reason = "Expected \"def\"", currentInput = "gzzz"})

>>> parse (firstThatParses [literal "abc", literal "def"] "errmsg") "zzzzzz"
Left (ParseError {reason = "errmsg", currentInput = "zzzzzz"})

>>> parse (firstThatParses [fail "test", pure 1, fail "test2"] "errmsg") "zzzzzz"
Right ("zzzzzz", 1)
-}
firstThatParses :: [Parser a] -> String -> Parser a
firstThatParses (p : ps) msg = Parser $ \s ->
    case parse p s of
        Left pe | currentInput pe == s -> parse (firstThatParses ps msg) s
        x -> x
firstThatParses [] msg = fail msg
