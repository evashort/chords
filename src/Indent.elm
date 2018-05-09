module Indent exposing (highlights, remove)

import Highlight exposing (Highlight)
import Substring exposing (Substring)

import Regex exposing (Regex, HowMany(..))

highlights : List Substring -> List Highlight
highlights lines =
  List.concatMap highlight lines

lineHighlights : Substring -> List Highlight
lineHighlights line =
  List.map
    (Highlight "#ffffff" "#ff0000")
    (Substring.find (AtMost 1) indentRegex line)

indentRegex : Regex
indentRegex = Regex.regex "^ +"

remove : List Substring -> List Substring
remove lines =
  List.filter unindented lines

unindented : Substring -> Bool
unindented line =
  not (String.startsWith " " line.s)
