module ChordParser exposing
  (Model, init, update, view, getChords, getSuggestions)

import CachedChord exposing (CachedChord)
import ChordFromCode exposing (chordFromCode)
import Highlight exposing (Highlight)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)

import Dict exposing (Dict)

type alias Model = List Word

init : List Substring -> Model
init = List.map parseChord

update : List Substring -> Model -> Model
update words model =
  List.map parseChord words

view : Model -> List Highlight
view = List.filterMap viewWord

getChords : Model -> List (List (Maybe CachedChord))
getChords model =
  List.filter
    (not << List.isEmpty)
    ( List.map
        (List.filterMap getChord)
        (splitList isNewline model)
    )

isNewline : Word -> Bool
isNewline word =
  word.substring.s == "\n"

splitList : (a -> Bool) -> List a -> List (List a)
splitList pred xs =
  let ( l, ls ) = splitListHelp pred xs in
    l :: ls

splitListHelp : (a -> Bool) -> List a -> ( List a, List (List a) )
splitListHelp pred xs =
  case xs of
    x :: rest ->
      let ( l, ls ) = splitListHelp pred rest in
        if pred x then
          ( [], l :: ls )
        else
          ( x :: l, ls )
    [] ->
      ( [], [] )

getSuggestions : Model -> List Suggestion
getSuggestions model =
  List.sortBy
    (.i << .firstRange)
    (Dict.values (List.foldl addSuggestion Dict.empty model))

addSuggestion :
  Word -> Dict String Suggestion -> Dict String Suggestion
addSuggestion word suggestions =
  case word.chord of
    Nothing ->
      suggestions
    Just chord ->
      if word.substring.s == chord.codeName then
        suggestions
      else
        Dict.update
          chord.codeName
          (updateSuggestion word chord)
          suggestions

updateSuggestion :
  Word -> CachedChord -> Maybe Suggestion -> Maybe Suggestion
updateSuggestion word chord maybeSuggestion =
  Just <|
    case maybeSuggestion of
      Nothing ->
        { s = chord.codeName
        , fg = CachedChord.fg chord
        , bg = CachedChord.bg chord
        , firstRange = word.substring
        , ranges = []
        }
      Just suggestion ->
        { suggestion
        | ranges = word.substring :: suggestion.ranges
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
        Just (Highlight "" "#808080" "#ffffff" word.substring)
      else
        Nothing
    Just chord ->
      if word.substring.s == chord.codeName then
        Just
          ( Highlight
              ""
              (CachedChord.fg chord)
              (CachedChord.bg chord)
              word.substring
          )
      else
        Nothing
