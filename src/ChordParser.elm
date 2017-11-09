module ChordParser exposing (Model, init, update, view, getChords)

import CachedChord exposing (CachedChord)
import ChordFromCode exposing (chordFromCode)
import Highlight exposing (Highlight)
import Substring exposing (Substring)

import Regex exposing (Regex, HowMany(..), Match)

type alias Model =
  { lines : List (List Word)
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

getChords : Model -> List (List (Maybe CachedChord))
getChords model =
  List.filter
    (not << List.isEmpty)
    (List.map (List.filterMap getChord) model.lines)

parse : List Substring -> Model
parse lines =
  let lineResults = List.map parseLine lines in
    { lines = List.map .words lineResults
    , indentation = List.filterMap .indentation lineResults
    }

type alias LineResult =
  { words : List Word
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

type alias Word =
  { substring : Substring
  , chord : Maybe CachedChord
  }

parseChord : Substring -> Word
parseChord substring =
  { substring = substring
  , chord = Maybe.map CachedChord.fromChord (chordFromCode substring.s)
  }

getChord : Word -> Maybe (Maybe CachedChord)
getChord word =
  case word.chord of
    Nothing ->
      if word.substring.s == "_" then
        Just Nothing
      else
        Nothing
    Just chord ->
      if word.substring.s == chord.codeName then
        Just (Just chord)
      else
        Nothing

viewWord : Word -> Maybe Highlight
viewWord word =
  case word.chord of
    Nothing ->
      if word.substring.s == "_" then
        Just (Highlight.fromSubstring "#808080" "#ffffff" word.substring)
      else
        Nothing
    Just chord ->
      if word.substring.s == chord.codeName then
        Just
          ( Highlight.fromSubstring
              (CachedChord.fg chord)
              (CachedChord.bg chord)
              word.substring
          )
      else
        Nothing
