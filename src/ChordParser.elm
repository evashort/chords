module ChordParser exposing (Model, init, update, view, getChords)

import Chord exposing (Chord)
import Highlight exposing (Highlight)
import Substring exposing (Substring)

import Regex exposing (Regex, HowMany(..), Match)

type alias Model =
  { words : List ChordResult
  , indentation : List Substring
  }

init : List Substring -> Model
init = parse

update : List Substring -> Model -> Model
update chordRanges model =
  init chordRanges

view : Model -> List Highlight
view model =
  List.filterMap viewWord model.words ++
    List.map Highlight.suggestDeletion model.indentation

getChords : Model -> List Chord
getChords model =
  List.filterMap .chord model.words

parse : List Substring -> Model
parse lines =
  let lineResults = List.map parseLine lines in
    { words = List.concatMap .words lineResults
    , indentation = List.filterMap .indentation lineResults
    }

type alias LineResult =
  { words : List ChordResult
  , indentation : Maybe Substring
  }

parseLine : Substring -> LineResult
parseLine line =
  case Regex.find (AtMost 1) (Regex.regex "^ +") line.s of
    match :: _ ->
      { words = []
      , indentation =
          Just (Substring.left (String.length match.match) line)
      }
    [] ->
      { words =
          List.map
            parseChord
            (Substring.regexSplit All (Regex.regex " +") line)
      , indentation = Nothing
      }

type alias ChordResult =
  { substring : Substring
  , chord : Maybe Chord
  }

parseChord : Substring -> ChordResult
parseChord word =
  { substring = word
  , chord = Chord.fromRawName word.s
  }

viewWord : ChordResult -> Maybe Highlight
viewWord word =
  case word.chord of
    Nothing ->
      Nothing
    Just c ->
      Just
        ( Highlight.fromSubstring
            (Chord.fg c)
            (Chord.bg c)
            word.substring
        )
