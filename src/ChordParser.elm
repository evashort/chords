module ChordParser exposing
  (IdChord, Model, init, update, view, getChords, getSuggestions, transpose)

import CachedChord exposing (CachedChord)
import ChordFromCode exposing (chordFromCode)
import Highlight exposing (Highlight)
import Replacement exposing (Replacement)
import Substring exposing (Substring)
import Suggestion exposing (Suggestion)
import Swatch exposing (Swatch)
import Zipper

type alias IdChord =
  { id : Int
  , cache : CachedChord
  }

type alias Model =
  { nextId : Int
  , lowestNote : Int
  , words : List Word
  }

type alias Word =
  { substring : Substring
  , chord : Maybe IdChord
  }

init : Int -> Int -> List Substring -> Model
init firstId lowestNote substrings =
  let
    ( words, nextId ) =
      List.foldr (parseChord lowestNote) ( [], firstId ) substrings
  in
    { nextId = nextId, lowestNote = lowestNote, words = words }

update : Int -> List Substring -> Model -> Model
update lowestNote substrings model =
  let
    lnOffset = lowestNote - model.lowestNote
  in let
    lnWords =
      if lnOffset == 0 then
        model.words
      else
        List.map
          ( mapChord
              (CachedChord.transposeRootOctave model.lowestNote lnOffset)
          )
          model.words
  in let
    doubleZipped =
      Zipper.doubleZip updateChord substrings lnWords
  in let
    ( newUpper, nextId ) =
      List.foldr
        (parseChord lowestNote)
        ( [], model.nextId )
        doubleZipped.upper
  in
    { nextId = nextId
    , lowestNote = lowestNote
    , words = doubleZipped.left ++ newUpper ++ doubleZipped.right
    }

view : Int -> Model -> List Highlight
view key model =
  List.filterMap (viewWord key model.lowestNote) model.words

getChords : Model -> List (List (Maybe IdChord))
getChords model =
  List.filter
    (not << List.isEmpty)
    ( List.map
        (List.filterMap (getChord model.lowestNote))
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

getSuggestions : Int -> Model -> List Suggestion
getSuggestions key model =
  Suggestion.groupByReplacement
    (List.filterMap (getSuggestion key model.lowestNote) model.words)

getSuggestion : Int -> Int -> Word -> Maybe ( List Swatch, Substring )
getSuggestion key lowestNote word =
  case word.chord of
    Nothing -> Nothing
    Just { cache } ->
      if word.substring.s == CachedChord.codeName lowestNote cache then
        Nothing
      else
        Just
          ( [ CachedChord.swatch key lowestNote cache ]
          , word.substring
          )

transpose : Int -> Model -> List Replacement
transpose offset model =
  List.filterMap (transposeWord model.lowestNote offset) model.words

transposeWord : Int -> Int -> Word -> Maybe Replacement
transposeWord lowestNote offset word =
  case word.chord of
    Nothing ->
      Nothing
    Just chord ->
      if word.substring.s == CachedChord.codeName lowestNote chord.cache then
        let
          newChord = List.map ((+) offset) chord.cache.chord
        in let
          newCache = CachedChord.fromChord newChord
        in
          Just
            ( Replacement
                word.substring
                (CachedChord.codeName (lowestNote + offset) newCache)
            )
      else
        Nothing

parseChord : Int -> Substring -> ( List Word, Int ) -> ( List Word, Int )
parseChord lowestNote substring ( rest, nextId ) =
  case chordFromCode lowestNote substring.s of
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

mapChord : (CachedChord -> CachedChord) -> Word -> Word
mapChord f word =
  case word.chord of
    Nothing ->
      word
    Just chord ->
      { word
      | chord = Just { chord | cache = f chord.cache }
      }

getChord : Int -> Word -> Maybe (Maybe IdChord)
getChord lowestNote word =
  case word.chord of
    Nothing ->
      if word.substring.s == "_" then
        Just Nothing
      else
        Nothing
    Just chord ->
      if word.substring.s == CachedChord.codeName lowestNote chord.cache then
        Just (Just chord)
      else
        Nothing

viewWord : Int -> Int -> Word -> Maybe Highlight
viewWord key lowestNote word =
  case word.chord of
    Nothing ->
      if word.substring.s == "_" then
        Just (Highlight "#808080" "#ffffff" word.substring)
      else
        Nothing
    Just chord ->
      if word.substring.s == CachedChord.codeName lowestNote chord.cache then
        Just
          ( Highlight
              (CachedChord.fg chord.cache)
              (CachedChord.bg key chord.cache)
              word.substring
          )
      else
        Nothing
