module X.Control.Parser.Combinator.WithTrailingWhitespace where
import X.Control.Parser.Text.Whitespace
import X.Control.Parser

-- Given a parser, returns a parser that swallows the whitespace after the parsed section.
withTrailingWhitespace :: Parser a -> Parser a
withTrailingWhitespace p = do
    v <- p
    whitespace
    return v