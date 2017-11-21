module ChordParser exposing
  (IdChord, Model, init, update, view, getChords, getSuggestions)

import CachedChord exposing (CachedChord)
import ChordFromCode exposing (chordFromCode)
import Highlight exposing (Highlight)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import Zipper

import Dict exposing (Dict)

type alias IdChord =
  { id : Int
  , cache : CachedChord
  }

type alias Model =
  { nextId : Int
  , words : List Word
  }

type alias Word =
  { substring : Substring
  , chord : Maybe IdChord
  }

init : List Substring -> Model
init substrings =
  let
    ( words, nextId ) =
      List.foldr parseChord ( [], 0 ) substrings
  in
    { nextId = nextId, words = words }

update : List Substring -> Model -> Model
update substrings model =
  let
    doubleZipped =
      Zipper.doubleZip updateChord substrings model.words
  in let
    ( newUpper, nextId ) =
      List.foldr parseChord ( [], model.nextId ) doubleZipped.upper
  in
    { nextId = nextId
    , words = doubleZipped.left ++ newUpper ++ doubleZipped.right
    }

view : Model -> List Highlight
view model =
  List.filterMap viewWord model.words

getChords : Model -> List (List (Maybe IdChord))
getChords model =
  List.filter
    (not << List.isEmpty)
    ( List.map
        (List.filterMap getChord)
        (splitList isNewline model.words)
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
    (Dict.values (List.foldl addSuggestion Dict.empty model.words))

addSuggestion :
  Word -> Dict String Suggestion -> Dict String Suggestion
addSuggestion word suggestions =
  case word.chord of
    Nothing ->
      suggestions
    Just chord ->
      if word.substring.s == chord.cache.codeName then
        suggestions
      else
        Dict.update
          chord.cache.codeName
          (updateSuggestion word chord.cache)
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

parseChord : Substring -> ( List Word, Int ) -> ( List Word, Int )
parseChord substring ( rest, nextId ) =
  case chordFromCode substring.s of
    Nothing ->
      ( { substring = substring
        , chord = Nothing
        } :: rest
      , nextId
      )
    Just chord ->
      ( { substring = substring
        , chord =
            Just
              { id = nextId
              , cache = CachedChord.fromChord chord
              }
        } :: rest
      , nextId + 1
      )

updateChord : Substring -> Word -> Maybe Word
updateChord substring word =
  if substring.s == word.substring.s then
    Just { word | substring = substring }
  else
    Nothing

getChord : Word -> Maybe (Maybe IdChord)
getChord word =
  case word.chord of
    Nothing ->
      if word.substring.s == "_" then
        Just Nothing
      else
        Nothing
    Just chord ->
      if word.substring.s == chord.cache.codeName then
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
      if word.substring.s == chord.cache.codeName then
        Just
          ( Highlight
              ""
              (CachedChord.fg chord.cache)
              (CachedChord.bg chord.cache)
              word.substring
          )
      else
        Nothing
