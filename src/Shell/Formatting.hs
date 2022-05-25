module Shell.Formatting (makeErrorMessageCmds, makeParseErrorCmds, makeValueCmds) where

-- holy shit this module is bad

import IO.IOCmd
import System.Console.ANSI
import Utils.String
import Utils.Trim
import Parser.Error

-- | Makes IOCmds for the lines surrounding the erroring line
_makeSurroundingErrorContextCmds :: Int -> [String] -> [IOCmd]
_makeSurroundingErrorContextCmds width sLines =
  coloredLine [SetColor Foreground Dull White] . trimLine width <$> sLines

{- | Make the width-adjusted erroring line and the carat pointing to the 0-indexed error location

## Examples

>>> uprint (a, b) = putStrLn a <> putStrLn b

>>> uprint $ _makeErroringLines 80 2 "abcde"
abcde
  ^

>>> uprint $ _makeErroringLines 5 2 "abcdef"
abcde
  ^

>>> uprint $ _makeErroringLines 5 3 "abcdefg"
bcdef
  ^

>>> uprint $ _makeErroringLines 5 10 "abcdefghijklmno"
ijklm
  ^
-}
_makeErroringLines :: Int -> Int -> String -> (String, String)
_makeErroringLines width errorIdx sLine =
  let (deletedFromLeft, formattedErrorLine, _) = trimAroundCenter errorIdx width sLine
      caratLine = replicate (errorIdx - deletedFromLeft) ' ' <> "^"
   in (formattedErrorLine, caratLine)

-- | Makes IOCmds for the erroring line itself
_makeErroringLineCmds :: Int -> Int -> String -> [IOCmd]
_makeErroringLineCmds width errorIdx sLine =
  let (formattedErrorLine, caratLine) = _makeErroringLines width errorIdx sLine
   in [ coloredLine [SetColor Foreground Vivid Red] formattedErrorLine
      , coloredLine [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] caratLine
      ]

-- | Makes IOCmds for the footer that shows the error location
_makeErrorLocationCmds :: Int -> String -> String -> [IOCmd]
_makeErrorLocationCmds width original sCurrentInput =
  let lengthNoNewlines = length . filter (/= '\n')
      ciPos = lengthNoNewlines original - lengthNoNewlines sCurrentInput
      (before, (errorLineLoc, errorLine), after) = splitIntoLinesByLocation ciPos original
      beforeLines = trimLine width . snd <$> before
      afterLines = trimLine width . snd <$> after

      errorIdx = ciPos - errorLineLoc

      upperCommands = _makeSurroundingErrorContextCmds width beforeLines
      errorLineCommands = _makeErroringLineCmds width errorIdx errorLine
      lowerCommands = _makeSurroundingErrorContextCmds width afterLines
   in upperCommands <> errorLineCommands <> lowerCommands


-- | Makes IOCmds for a successfully calculated value
makeValueCmds :: String -> [IOCmd]
makeValueCmds v = [coloredLine [SetColor Foreground Vivid Blue] v]

-- | Makes IOCmds for the `error: xxxxx` line
makeErrorMessageCmds :: String -> [IOCmd]
makeErrorMessageCmds errmsg =
  [ coloredText [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] "error: "
  , coloredLine [SetColor Foreground Vivid Red] errmsg
  ]

-- | Given the terminal width, input, and parse error, makes commands for printing it
makeParseErrorCmds :: Int -> String -> ParseError -> [IOCmd]
makeParseErrorCmds width original (ParseError r ci) =
    makeErrorMessageCmds r <> [newline] <> _makeErrorLocationCmds width original ci
