module ChordParser exposing (Model, init, update, view, getChords)

import Chord exposing (Chord)
import Highlight exposing (Highlight)
import Substring exposing (Substring)

import Regex exposing (Regex, HowMany(..), Match)

type alias Model = List ChordResult

init : List Substring -> Model
init = parse

update : List Substring -> Model -> Model
update chordRanges model =
  init chordRanges

view : Model -> List Highlight
view = List.filterMap viewChordResult

getChords : Model -> List Chord
getChords = List.filterMap .chord

parse : List Substring -> Model
parse = List.concatMap parseLine

parseLine : Substring -> List ChordResult
parseLine = List.map parseChord << Substring.regexSplit All (Regex.regex " +")

type alias ChordResult =
  { substring : Substring
  , chord : Maybe Chord
  }

parseChord : Substring -> ChordResult
parseChord word =
  { substring = word
  , chord = Chord.fromRawName word.s
  }

viewChordResult : ChordResult -> Maybe Highlight
viewChordResult chordResult =
  case chordResult.chord of
    Nothing ->
      Nothing
    Just c ->
      Just
        ( Highlight.fromSubstring
            (Chord.fg c)
            (Chord.bg c)
            chordResult.substring
        )
