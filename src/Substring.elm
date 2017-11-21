module Substring exposing
  (Substring, stop, dropLeft, dropRight, before, after, find)

import Regex exposing (Regex, HowMany(..), Match)

type alias Substring =
  { i : Int
  , s : String
  }

stop : Substring -> Int
stop { i, s } =
  i + String.length s

dropLeft : Int -> Substring -> Substring
dropLeft n substring =
  let s = String.dropLeft n substring.s in
    { i = stop substring - String.length s, s = s }

dropRight : Int -> Substring -> Substring
dropRight n { i, s } =
  { i = i, s = String.dropRight n s }

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
