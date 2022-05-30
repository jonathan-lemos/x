module Utils.Trim where
import Data.Char

{- | Drops elements from the longer list (left if they're equal) until the sum of their lengths is <= `minWidth`

Returns `(amount of elements dropped from left, left, amount of elements dropped from right, right)`

## Examples

>>> balance 5 "abcde" "fg"
(2,"cde",0,"fg")

>>> balance 5 "ab" "cdefg"
(0,"ab",2,"efg")

>>> balance 6 "abcde" "fghi"
(2,"cde",1,"ghi")

>>> balance 7 "abcd" "eghijk"
(1,"bcd",2,"hijk")

>>> balance 10 "abcdefg" "hij"
(0,"abcdefg",0,"hij")
-}
balance :: Int -> [a] -> [a] -> (Int, [a], Int, [a])
balance width lt gte =
    let go lw l rw r
            | lw + rw <= width = (length lt - lw, l, length gte - rw, r)
            | lw == 0 && rw == 0 = (0, [], 0, [])
            | rw == 0 || lw >= rw = go (lw - 1) (tail l) rw r
            | otherwise = go lw l (rw - 1) (tail r)
     in go (length lt) lt (length gte) gte

{- | Trims a `line` around a 0-indexed `center` character until the given `maxWidth`.

Returns `(number of characters removed from left side, trimmed string, number of characters removed from right side)`

## Examples

>>> trimAroundCenter 3 6 "abcDefghij"
(0,"abcDef",4)

>>> trimAroundCenter 7 6 "abcdefgHij"
(4,"efgHij",0)

>>> trimAroundCenter 6 6 "abcdefGhij"
(3,"defGhi",1)
-}
trimAroundCenter :: Int -> Int -> String -> (Int, String, Int)
trimAroundCenter center maxWidth line =
    let (lt, gte) = splitAt center line
        (leftChopped, left, rightChopped, right) = balance maxWidth lt (reverse gte)
     in (leftChopped, left <> reverse right, rightChopped)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
