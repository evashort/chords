module MainParser exposing
  (Model, init, update, view, getChords, getSuggestions)

import CachedChord exposing (CachedChord)
import ChordParser
import Highlight exposing (Highlight)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)

import Regex exposing (Regex, HowMany(..), Match)

type alias Model =
  { chordModel : ChordParser.Model
  , comments : List Substring
  , indents : List Substring
  }

init : Substring -> Model
init substring =
  let parseResult = parse substring in
    { chordModel =
        ChordParser.init parseResult.words
    , comments = parseResult.comments
    , indents = parseResult.indents
    }

update : Substring -> Model -> Model
update whole model =
  let parseResult = parse whole in
    { chordModel =
        ChordParser.update parseResult.words model.chordModel
    , comments = parseResult.comments
    , indents = parseResult.indents
    }

view : Model -> List Highlight
view model =
  ChordParser.view model.chordModel ++
    List.map (Highlight "" "#008000" "#ffffff") model.comments ++
      List.map (Highlight "" "#ffffff" "#ff0000") model.indents

getChords : Model -> List (List (Maybe CachedChord))
getChords = ChordParser.getChords << .chordModel

getSuggestions : Model -> List Suggestion
getSuggestions = ChordParser.getSuggestions << .chordModel

type alias ParseResult =
  { words : List Substring
  , comments : List Substring
  , indents : List Substring
  }

parse : Substring -> ParseResult
parse whole =
  let
    lineResults =
      List.map
        parseLine
        (Substring.find All (Regex.regex ".*\n?") whole)
  in
    { words = List.concatMap .words lineResults
    , comments = List.filterMap .comment lineResults
    , indents = List.filterMap .indent lineResults
    }

type alias LineResult =
  { words : List Substring
  , comment : Maybe Substring
  , indent : Maybe Substring
  }

parseLine : Substring -> LineResult
parseLine line =
  case Substring.find (AtMost 1) (Regex.regex "^#.*") line of
    comment :: _ ->
      { words = []
      , comment = Just comment
      , indent = Nothing
      }
    [] ->
      let
        comment =
          case Substring.find (AtMost 1) (Regex.regex " #.*") line of
            spaceAndComment :: _ ->
              Just (Substring.dropLeft 1 spaceAndComment)
            [] ->
              Nothing
      in let
        code =
          case comment of
            Just c -> Substring.before c.i line
            Nothing -> line
      in
        case
          Substring.find (AtMost 1) (Regex.regex "^ +[^ \n]") code
        of
          indentAndChar :: _ ->
            { words = []
            , comment = comment
            , indent = Just (Substring.dropRight 1 indentAndChar)
            }
          [] ->
            let
              words = Substring.find All (Regex.regex "[^ \n]+") code
            in
              { words =
                  if words == [] then
                    []
                  else
                    words ++
                      Substring.find (AtMost 1) (Regex.regex "\n$") line
              , comment = comment
              , indent = Nothing
              }
