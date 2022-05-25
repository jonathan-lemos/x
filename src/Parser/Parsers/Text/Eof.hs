module Parser.Parsers.Text.Eof where
import Parser.Parser
import Parser.Error

eof :: Parser ()
eof =
    let parseEof "" = Right ("", ())
        parseEof s  = Left (ParseError "Expected end-of-input" s)
    in Parser parseEof
