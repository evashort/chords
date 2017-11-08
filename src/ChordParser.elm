module ChordParser exposing (Model, init, update, view, getChords)

import Chord exposing (Chord)
import ChordFromCode exposing (chordFromCode)
import Highlight exposing (Highlight)
import Substring exposing (Substring)

import Regex exposing (Regex, HowMany(..), Match)

type alias Model =
  { lines : List (List ChordResult)
  , indentation : List Substring
  }

init : List Substring -> Model
init = parse

update : List Substring -> Model -> Model
update chordRanges model =
  init chordRanges

view : Model -> List Highlight
view model =
  List.concatMap (List.filterMap viewWord) model.lines ++
    List.map Highlight.suggestDeletion model.indentation

getChords : Model -> List (List (Maybe Chord))
getChords model =
  List.filter
    (not << List.isEmpty)
    (List.map (List.filterMap .chord) model.lines)

parse : List Substring -> Model
parse lines =
  let lineResults = List.map parseLine lines in
    { lines = List.map .words lineResults
    , indentation = List.filterMap .indentation lineResults
    }

type alias LineResult =
  { words : List ChordResult
  , indentation : Maybe Substring
  }

parseLine : Substring -> LineResult
parseLine line =
  case Substring.find (AtMost 1) (Regex.regex "^ +") line of
    indentation :: _ ->
      { words = []
      , indentation = Just indentation
      }
    [] ->
      { words =
          List.map
            parseChord
            (Substring.find All (Regex.regex "[^ ]+") line)
      , indentation = Nothing
      }

type alias ChordResult =
  { substring : Substring
  , chord : Maybe (Maybe Chord)
  }

parseChord : Substring -> ChordResult
parseChord word =
  { substring = word
  , chord =
      if word.s == "_" then
        Just Nothing
      else
        Maybe.map Just (chordFromCode word.s)
  }

viewWord : ChordResult -> Maybe Highlight
viewWord word =
  case word.chord of
    Just (Just c) ->
      Just
        (Highlight.fromSubstring (Chord.fg c) (Chord.bg c) word.substring)
    Just Nothing ->
      Just
        (Highlight.fromSubstring "#808080" "#ffffff" word.substring)
    Nothing ->
      Nothing
