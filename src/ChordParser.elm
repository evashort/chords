module ChordParser exposing (Model, init, update, view)

import Highlight exposing (Highlight)
import Substring exposing (Substring)

import Regex exposing (Regex, HowMany(..), Match)

type alias Model = List Substring

init : List Substring -> Model
init = parse

update : List Substring -> Model -> Model
update chordRanges model =
  init chordRanges

view : Model -> List Highlight
view = List.filterMap viewSubstring

parse : List Substring -> Model
parse = List.concatMap parseLine

parseLine : Substring -> List Substring
parseLine = Substring.regexSplit All (Regex.regex " +")

viewSubstring : Substring -> Maybe Highlight
viewSubstring substring =
  let
    fg =
      case substring.s of
        "Bo" -> "#ffffff"
        _ -> "#000000"
  in let
    bg =
      case substring.s of
        "C" -> "#f8facd"
        "Dm" -> "#eccdfa"
        "Em" -> "#d2facd"
        "F" -> "#facdcd"
        "G" -> "#c9ffff"
        "Am" -> "#ffe7c9"
        "Bo" -> "#005e93"
        _ -> "#ffffff"
  in
    case ( fg, bg ) of
      ( "#000000", "#ffffff" ) -> Nothing
      _ -> Just (Highlight.fromSubstring fg bg substring)
