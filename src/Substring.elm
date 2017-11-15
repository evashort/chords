module Substring exposing
  ( Substring, stop, lines, left, dropLeft, before, after, find )

import Regex exposing (Regex, HowMany(..), Match)

type alias Substring =
  { i : Int
  , s : String
  }

stop : Substring -> Int
stop { i, s } =
  i + String.length s

lines : Substring -> List Substring
lines = regexSplit All (Regex.regex "\\r\\n|\\r|\\n")

left : Int -> Substring -> Substring
left n { i, s } =
  { i = i, s = String.left n s }

dropLeft : Int -> Substring -> Substring
dropLeft n substring =
  let s = String.dropLeft n substring.s in
    { i = stop substring - String.length s, s = s }

before : Int -> Substring -> Substring
before x { i, s } =
  { i = i, s = String.left (x - i) s }

after : Int -> Substring -> Substring
after x substring =
  let s = String.dropLeft (x - substring.i) substring.s in
    { i = stop substring - String.length s, s = s }

find : HowMany -> Regex -> Substring -> List Substring
find howMany regex { i, s } =
  List.map (fromMatch i) (Regex.find howMany regex s)

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
