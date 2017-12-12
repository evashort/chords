module ChordParser exposing
  (IdChord, Model, init, update, view, getChords, getSuggestions)

import CachedChord exposing (CachedChord)
import Chord
import ChordFromCode exposing (chordFromCode)
import Highlight exposing (Highlight)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import Swatch exposing (Swatch)
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
updateSuggestion word cache maybeSuggestion =
  Just <|
    case maybeSuggestion of
      Nothing ->
        { replacement = cache.codeName
        , swatchLists =
            let
              ( a, b, c ) = cache.flavor.bg
            in let
              ( d, e, f ) =
                case Chord.get cache.chord cache.i % 3 of
                  0 -> ( a, b, c )
                  1 -> ( b, c, a )
                  _ -> ( c, a, b )
            in
              ( [ Swatch cache.flavor.fg d cache.codeName ]
              , [ Swatch cache.flavor.fg e cache.codeName ]
              , [ Swatch cache.flavor.fg f cache.codeName ]
              )
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
