module Shell.Formatting (makeErrorMessageCmds, makeErrorLocationCmds, makeValueCmds) where

-- holy shit this module is bad

import IO.IOCmd
import System.Console.ANSI
import Utils.List
import Utils.String
import Utils.Trim (trimAroundCenter)

-- | Makes IOCmds for a successfully calculated value
makeValueCmds :: String -> [IOCmd]
makeValueCmds v = [coloredLine [SetColor Foreground Vivid Blue] v]

-- | Makes IOCmds for the `error: xxxxx` line
makeErrorMessageCmds :: String -> [IOCmd]
makeErrorMessageCmds errmsg =
  [ coloredText [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] "error: "
  , coloredLine [SetColor Foreground Vivid Red] errmsg
  ]

-- | Makes IOCmds for the lines surrounding the erroring line
makeSurroundingErrorContextCmds :: Int -> [String] -> [IOCmd]
makeSurroundingErrorContextCmds width lines =
  coloredLine [SetColor Foreground Dull White] . trimLine width <$> lines

{- | Make the width-adjusted erroring line and the carat pointing to the 0-indexed error location

## Examples

>>> uprint (a, b) = putStrLn a <> putStrLn b

>>> uprint $ makeErroringLines 80 2 "abcde"
abcde
  ^

>>> uprint $ makeErroringLines 5 2 "abcdef"
abcde
  ^

>>> uprint $ makeErroringLines 5 3 "abcdefg"
bcdef
  ^

>>> uprint $ makeErroringLines 5 10 "abcdefghijklmno"
ijklm
  ^
-}
makeErroringLines :: Int -> Int -> String -> (String, String)
makeErroringLines width errorIdx line =
  let (deletedFromLeft, formattedErrorLine, _) = trimAroundCenter errorIdx width line
      caratLine = replicate (errorIdx - deletedFromLeft) ' ' <> "^"
   in (formattedErrorLine, caratLine)

-- | Makes IOCmds for the erroring line itself
makeErroringLineCmds :: Int -> Int -> String -> [IOCmd]
makeErroringLineCmds width errorIdx line =
  let (formattedErrorLine, caratLine) = makeErroringLines width errorIdx line
   in [ coloredLine [SetColor Foreground Vivid Red] formattedErrorLine
      , coloredLine [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] caratLine
      ]

-- | Makes IOCmds for the footer that shows the error location
makeErrorLocationCmds :: Int -> String -> String -> [IOCmd]
makeErrorLocationCmds width original currentInput =
  let lengthNoNewlines = length . filter (/= '\n')
      ciPos = lengthNoNewlines original - lengthNoNewlines currentInput
      (before, (errorLineLoc, errorLine), after) = splitIntoLinesByLocation ciPos original
      beforeLines = trimLine width . snd <$> before
      afterLines = trimLine width . snd <$> after

      errorIdx = ciPos - errorLineLoc

      upperCommands = makeSurroundingErrorContextCmds width beforeLines
      errorLineCommands = makeErroringLineCmds width errorIdx errorLine
      lowerCommands = makeSurroundingErrorContextCmds width afterLines
   in upperCommands <> errorLineCommands <> lowerCommands
