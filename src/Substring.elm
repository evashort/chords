module Substring exposing
  (Substring, fromString, stop, lines, left, dropLeft, find)

import Regex exposing (Regex, HowMany(..), Match)

type alias Substring =
  { i : Int
  , s : String
  }

fromString : String -> Substring
fromString = Substring 0

stop : Substring -> Int
stop substring =
  substring.i + String.length substring.s

lines : Substring -> List Substring
lines = regexSplit All (Regex.regex "\\r\\n|\\r|\\n")

left : Int -> Substring -> Substring
left n substring =
  { i = substring.i, s = String.left n substring.s }

dropLeft : Int -> Substring -> Substring
dropLeft n substring =
  { i = substring.i + n, s = String.dropLeft n substring.s }

find : HowMany -> Regex -> Substring -> List Substring
find howMany regex substring =
  List.map
    (fromMatch substring.i)
    (Regex.find howMany regex substring.s)

fromMatch : Int -> Match -> Substring
fromMatch i match =
  { i = i + match.index, s = match.match }

regexSplit : HowMany -> Regex -> Substring -> List Substring
regexSplit howMany regex substring =
  let
    matches =
      Regex.find howMany regex substring.s ++
        [ emptyMatch (String.length substring.s) ]
  in
    List.map2 (betweenMatches substring) (emptyMatch 0 :: matches) matches

emptyMatch : Int -> Match
emptyMatch index =
  { match = "", submatches = [], index = index, number = 0 }

betweenMatches : Substring -> Match -> Match -> Substring
betweenMatches substring x y =
  let xStop = x.index + String.length x.match in
    { i = substring.i + xStop
    , s = String.slice xStop y.index substring.s
    }
