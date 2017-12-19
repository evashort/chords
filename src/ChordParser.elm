module ChordParser exposing
  (IdChord, Model, init, update, view, getChords, getSuggestions)

import CachedChord exposing (CachedChord)
import ChordFromCode exposing (chordFromCode)
import Highlight exposing (Highlight)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import Swatch exposing (Swatch)
import Zipper

import Dict exposing (Dict)
import Set exposing (Set)

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

init : Int -> List Substring -> Model
init firstId substrings =
  let
    ( words, nextId ) =
      List.foldr parseChord ( [], firstId ) substrings
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

view : Int -> Model -> List Highlight
view key model =
  List.filterMap (viewWord key) model.words

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

getSuggestions :
  Int -> Model -> ( List Suggestion, Dict String (Set ( Int, Int )) )
getSuggestions key model =
  let
    suggestions =
      Suggestion.groupByReplacement
        (List.filterMap (getSuggestion key) model.words)
  in
    ( Dict.values suggestions
    , Dict.map (always Suggestion.rangeSet) suggestions
    )

getSuggestion : Int -> Word -> Maybe ( List Swatch, Substring )
getSuggestion key word =
  case word.chord of
    Nothing -> Nothing
    Just { cache } ->
      if word.substring.s == cache.codeName then Nothing
      else Just ( [ CachedChord.swatch key cache ], word.substring )

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

viewWord : Int -> Word -> Maybe Highlight
viewWord key word =
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
              (CachedChord.bg key chord.cache)
              word.substring
          )
      else
        Nothing
