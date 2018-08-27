module Substring exposing
  ( Substring, stop, range, left, right, dropLeft, dropRight, before, after
  , between, find, regexSplit
  )

import Regex exposing (Regex, Match)

type alias Substring =
  { i : Int
  , s : String
  }

stop : Substring -> Int
stop { i, s } =
  i + String.length s

range : Substring -> ( Int, Int )
range { i, s } =
  ( i, i + String.length s )

left : Int -> Substring -> Substring
left n { i, s } =
  { i = i, s = String.left n s }

right : Int -> Substring -> Substring
right n substring =
  let s = String.right n substring.s in
    { i = stop substring - String.length s, s = s }

dropLeft : Int -> Substring -> Substring
dropLeft n substring =
  let s = String.dropLeft n substring.s in
    { i = stop substring - String.length s, s = s }

dropRight : Int -> Substring -> Substring
dropRight n { i, s } =
  { i = i, s = String.dropRight n s }

before : Int -> Substring -> String
before x { i, s } =
  String.left (x - i) s

after : Int -> Substring -> Substring
after x { i, s } =
  { i = max i x, s = String.dropLeft (x - i) s }

between : Int -> Int -> Substring -> Substring
between x y { i, s } =
  let start = max 0 (x - i) in
    { i = i + start
    , s = String.slice start (max start (y - i)) s
    }

find : Regex -> Substring -> List Substring
find regex { i, s } =
  List.map (fromMatch i) (Regex.find regex s)

fromMatch : Int -> Match -> Substring
fromMatch i match =
  { i = i + match.index, s = match.match }

regexSplit : Regex -> Substring -> List Substring
regexSplit regex substring =
  let
    matches =
      Regex.find regex substring.s ++
        [ emptyMatch (String.length substring.s) ]
  in
    List.map2 (betweenMatches substring) (emptyMatch 0 :: matches) matches

emptyMatch : Int -> Match
emptyMatch index =
  { match = "", submatches = [], index = index, number = 0 }

betweenMatches : Substring -> Match -> Match -> Substring
betweenMatches { i, s } x y =
  let xStop = x.index + String.length x.match in
    { i = i + xStop
    , s = String.slice xStop y.index s
    }
