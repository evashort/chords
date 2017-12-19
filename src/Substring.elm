module Substring exposing
  (Substring, stop, range, left, right, dropLeft, dropRight, after, find)

import Regex exposing (Regex, HowMany(..), Match)

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
