module Tokenizer exposing (tokenize)

import Regex exposing (Regex, HowMany(..), Match)

tokenize : String -> List String
tokenize text =
  List.concatMap tokenizeCommentedLine (String.lines text)

tokenizeCommentedLine : String -> List String
tokenizeCommentedLine line =
  if String.startsWith "#" line then
    [ line, "\n" ]
  else
    case Regex.find (AtMost 1) (Regex.regex "\\s+#") line of
      [ match ] ->
        tokenizeLine (String.left match.index line) ++
          [ String.dropRight 1 match.match
          , String.dropLeft (match.index + String.length match.match - 1) line
          , "\n"
          ]
      _ ->
        tokenizeLine line ++ [ "\n" ]

tokenizeLine : String -> List String
tokenizeLine text =
  let matches = Regex.find All (Regex.regex "\\s+") text in
    case matches of
      x :: _ ->
        case String.left x.index text of
          "" -> fromMatches matches text
          firstToken -> firstToken :: fromMatches matches text
      [] ->
        case text of
          "" -> []
          onlyToken -> [ onlyToken ]

fromMatches : List Match -> String -> List String
fromMatches matches text =
  case matches of
    x :: y :: rest ->
      x.match :: betweenMatches x y text :: fromMatches (y :: rest) text
    [ x ] ->
      x.match ::
        case String.dropLeft (x.index + String.length x.match) text of
          "" -> []
          lastToken -> [ lastToken ]
    [] ->
      []

betweenMatches : Match -> Match -> String -> String
betweenMatches x y =
  String.slice (x.index + String.length x.match) y.index
