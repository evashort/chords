module Comment exposing (highlights, remove)

import Highlight exposing (Highlight)
import Substring exposing (Substring)

import Regex exposing (Regex, HowMany(..))

highlights : Substring -> List Highlight
highlights whole =
  case Substring.find (AtMost 1) atStartRegex whole of
    [] ->
      List.map
        (highlight << Substring.dropLeft 1)
        (Substring.find All commentRegex whole)
    firstLine :: _ ->
      let
        rest =
          Substring.dropLeft (String.length firstLine.s) whole
      in
        highlight firstLine ::
          List.map
            (highlight << Substring.dropLeft 1)
            (Substring.find All commentRegex rest)

highlight : Substring -> Highlight
highlight = Highlight "#008000" "#ffffff"

atStartRegex : Regex
atStartRegex = Regex.regex "^#.*"

commentRegex : Regex
commentRegex = Regex.regex "[ \\n]#.*"

codeRegex : Regex
codeRegex = Regex.regex ".*[^ \\n]"

remove : Substring -> List Substring
remove whole =
  let
    rest =
      case Regex.find (AtMost 1) atStartRegex whole.s of
        [] ->
          whole
        match :: _ ->
          Substring.dropLeft (String.length match.match) whole
  in let
    betweenComments =
      Substring.regexSplit commentRegex rest
  in
    List.concatMap
      (Substring.find All codeRegex)
      betweenComments
