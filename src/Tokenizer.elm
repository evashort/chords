module Tokenizer exposing (tokenize)

import Regex exposing (Regex, HowMany(..), Match)

tokenize : String -> List String
tokenize text =
  let matches = Regex.find All whitespace text in
    case matches of
      x :: _ ->
        case String.left x.index text of
          "" -> fromMatches matches text
          firstToken -> firstToken :: fromMatches matches text
      [] ->
        case text of
          "" -> []
          onlyToken -> [ onlyToken ]

whitespace : Regex
whitespace =
  Regex.regex "\\s+"

betweenMatches : Match -> Match -> String -> String
betweenMatches x y =
  String.slice (x.index + String.length x.match) y.index

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
