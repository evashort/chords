module MainParser exposing (Model, init, update, view, getChords)

import Chord exposing (Chord)
import ChordParser
import Highlight exposing (Highlight)
import Substring exposing (Substring)

import Regex exposing (Regex, HowMany(..), Match)

type alias Model =
  { chordModel : ChordParser.Model
  , comments : List Substring
  }

init : Substring -> Model
init substring =
  let parseResult = parse substring in
    { chordModel =
        ChordParser.init parseResult.chordRanges
    , comments = parseResult.comments
    }

update : Substring -> Model -> Model
update substring model =
  let parseResult = parse substring in
    { chordModel =
        ChordParser.update parseResult.chordRanges model.chordModel
    , comments = parseResult.comments
    }

view : Model -> List Highlight
view model =
  ChordParser.view model.chordModel ++
    List.map (Highlight.fromSubstring "#008000" "#ffffff") model.comments

getChords : Model -> List (List Chord)
getChords = ChordParser.getChords << .chordModel

type alias ParseResult =
  { chordRanges : List Substring
  , comments : List Substring
  }

parse : Substring -> ParseResult
parse substring =
  let
    lineResults = List.map parseLine (Substring.lines substring)
  in
    { chordRanges = List.filterMap .chordRange lineResults
    , comments = List.filterMap .comment lineResults
    }

type alias LineResult =
  { chordRange : Maybe Substring
  , comment : Maybe Substring
  }

parseLine : Substring -> LineResult
parseLine line =
  if String.startsWith "#" line.s then
    { chordRange = Nothing
    , comment = Just line
    }
  else
    case Regex.find (AtMost 1) (Regex.regex " +#") line.s of
      match :: _ ->
        { chordRange = Just (Substring.left match.index line)
        , comment =
            Just
              ( Substring.dropLeft
                  (match.index + String.length match.match - 1)
                  line
              )
        }
      [] ->
        { chordRange =
            case Regex.find (AtMost 1) (Regex.regex " +$") line.s of
              match :: _ ->
                let beforeSpace = Substring.left match.index line in
                  case beforeSpace.s of
                    "" -> Nothing
                    _ -> Just beforeSpace
              [] ->
                Just line
        , comment = Nothing
        }
