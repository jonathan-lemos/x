module X.Utils.String where

import Data.Bifunctor
import X.Utils.List

{- | Turns a list of strings into an English string

## Examples

>>> pluralize []
""

>>> pluralize ["abc"]
"abc"

>>> pluralize ["abc", "def"]
"abc or def"

>>> pluralize ["abc", "def", "ghi"]
"abc, def, or ghi"

>>> pluralize ["abc", "def", "ghi", "ijk"]
"abc, def, ghi, or ijk"

-}
pluralize :: [String] -> String
pluralize [] = ""
pluralize [x] = x
pluralize [x, y] = x <> " or " <> y
pluralize lst =
    let go [] = ""
        go [x] = "or " <> x
        go (x: xs) = x <> ", " <> go xs
     in go lst

{- | Constricts a string from a given startIndex to a given width, adding elipses if either end is trimmed.
The integer in the return value represents the **net** number of characters removed *from the front* (replacing with elipsis does not remove a character).

## Examples

-- by default, show escapes … as \8230
>>> print (a, b) = putStrLn $ show a <> "," <> b

>>> print $ constrict 0 10 "abcde"
0,abcde

>>> print $ constrict 0 5 "abcde"
0,abcde

>>> print $ constrict 0 5 "abcdefg"
0,abcd…

>>> print $ constrict 1 5 "abcdefg"
0,…bcd…

>>> print $ constrict 2 5 "abcdefg"
1,…cde…

>>> print $ constrict 3 5 "abcdefg"
2,…defg

>>> print $ constrict 4 5 "abcdefg"
3,…efg

>>> print $ constrict 5 5 "abcdefg"
4,…fg

>>> print $ constrict 6 5 "abcdefg"
5,…g

>>> print $ constrict 8 5 "abcdefghijklmno"
7,…ijk…

-}
constrict :: Int -> Int -> String -> (Int, String)
constrict 0 maxWidth line =
    if length line > maxWidth
        then (0, take (maxWidth - 1) line <> "…")
        else (0, line)
constrict start maxWidth line =
    first (const $ start - 1) <$> constrict 0 maxWidth $ '…' : drop start line


{- | Trims a string from the end to a given length, adding an elipsis if the end is cut off.

## Examples

>>> putStrLn $ trimLine 5 "abcde"
abcde

>>> putStrLn $ trimLine 10 "abcde"
abcde

>>> putStrLn $ trimLine 5 "abcdef"
abcd…

-}
trimLine :: Int -> String -> String
trimLine n s = snd $ constrict 0 n s

{- | Splits a line into (lines before location, line containing location, lines after location).
The location is in 0-indexed characters, not including newlines.

## Examples

>>> splitIntoLinesByLocation 0 "abc"
([],(0,"abc"),[])

>>> splitIntoLinesByLocation 7 "abc\ndef\nghi\njkl\nmno"
([(0,"abc"),(3,"def")],(6,"ghi"),[(9,"jkl"),(12,"mno")])
-}
splitIntoLinesByLocation :: Int -> String -> ([(Int, String)], (Int, String), [(Int, String)])
splitIntoLinesByLocation location text =
    let indexLines = indexes (if null text then [""] else lines text)
        beforeOrEqual = filter ((<= location) . fst) indexLines
        after = filter ((> location) . fst) indexLines
        targetLine = last beforeOrEqual
        before = take (length beforeOrEqual - 1) beforeOrEqual
     in (before, targetLine, after)

parenthesize :: String -> String
parenthesize = ("(" <>) . (<> ")")
