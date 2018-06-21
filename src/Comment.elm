module Comment exposing (highlights, remove)

import Highlight exposing (Highlight)
import Substring exposing (Substring)

import Regex exposing (Regex, HowMany(..))

highlights : Substring -> List Highlight
highlights whole =
  case Substring.find (AtMost 1) initialCommentRegex whole of
    [] ->
      List.map
        (highlight << Substring.dropLeft 1)
        (Substring.find All commentRegex whole)
    initialComment :: _ ->
      let
        rest =
          Substring.dropLeft (String.length initialComment.s) whole
      in
        highlight initialComment ::
          List.map
            (highlight << Substring.dropLeft 1)
            (Substring.find All commentRegex rest)

initialCommentRegex : Regex
initialCommentRegex = Regex.regex "^\\/\\/.*"

commentRegex : Regex
commentRegex = Regex.regex "[ \\n]\\/\\/.*"

highlight : Substring -> Highlight
highlight = Highlight "#008000" "#ffffff"

remove : Substring -> List Substring
remove whole =
  List.map
    removeFromLine
    (Substring.regexSplit All lineBreak whole)
  ++ [ Substring (String.length whole.s + 1) "" ] -- Needed by Flag.insert

removeFromLine : Substring -> Substring
removeFromLine line =
  if String.startsWith "//" line.s then
    { line | s = "" }
  else
    case Regex.find (AtMost 1) commentStart line.s of
      match :: _ ->
        Substring.left match.index line
      _ ->
        line

lineBreak : Regex
lineBreak = Regex.regex " *\\n"

commentStart : Regex
commentStart = Regex.regex " +\\/\\/"
