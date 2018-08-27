module Comment exposing (highlights, remove)

import Highlight exposing (Highlight)
import Substring exposing (Substring)

import Regex exposing (Regex)

highlights : Substring -> List Highlight
highlights whole =
  case Substring.find initialCommentRegex whole of
    [] ->
      List.map
        (highlight << Substring.dropLeft 1)
        (Substring.find commentRegex whole)
    initialComment :: _ ->
      let
        rest =
          Substring.dropLeft (String.length initialComment.s) whole
      in
        highlight initialComment ::
          List.map
            (highlight << Substring.dropLeft 1)
            (Substring.find commentRegex rest)

initialCommentRegex : Regex
initialCommentRegex =
  Maybe.withDefault Regex.never (Regex.fromString "^\\/\\/.*")

commentRegex : Regex
commentRegex =
  Maybe.withDefault Regex.never (Regex.fromString "[ \\n]\\/\\/.*")

highlight : Substring -> Highlight
highlight = Highlight "#008000" "#ffffff"

remove : Substring -> List Substring
remove whole =
  let
    wholeAndNewline = -- let Flag.insert know where the string ends
      { whole | s = whole.s ++ "\n" }
  in
    List.map
      removeFromLine
      (Substring.regexSplit lineBreak wholeAndNewline)

removeFromLine : Substring -> Substring
removeFromLine line =
  if String.startsWith "//" line.s then
    { line | s = "" }
  else
    case Regex.findAtMost 1 commentStart line.s of
      match :: _ ->
        Substring.left match.index line
      _ ->
        line

lineBreak : Regex
lineBreak =
  Maybe.withDefault Regex.never (Regex.fromString " *\\n")

commentStart : Regex
commentStart =
  Maybe.withDefault Regex.never (Regex.fromString " +\\/\\/")
