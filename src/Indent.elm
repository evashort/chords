module Indent exposing (highlights, remove)

import Highlight exposing (Highlight)
import Substring exposing (Substring)

import Regex exposing (Regex)

highlights : List Substring -> List Highlight
highlights lines =
  List.concatMap lineHighlights lines

lineHighlights : Substring -> List Highlight
lineHighlights line =
  List.map
    (Highlight "#ffffff" "#ff0000")
    (Substring.find indentRegex line)

indentRegex : Regex
indentRegex =
  Maybe.withDefault Regex.never (Regex.fromString "^ +")

remove : List Substring -> List Substring
remove lines =
  List.filter unindented lines

unindented : Substring -> Bool
unindented line =
  not (String.startsWith " " line.s)
