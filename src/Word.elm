module Word exposing
  (Word, init, update, highlight, meaning, suggestion, mapChord)

import Chord exposing (Chord)
import Colour
import Highlight exposing (Highlight)
import Name
import PlayStatus exposing (IdChord)
import Substring exposing (Substring)
import Swatch exposing (Swatch)

type alias Word =
  { id : Int
  , substring : Substring
  , cache : Maybe Cache
  }

type alias Cache =
  { chord : Chord
  , codeName : String
  }

init : Int -> Substring -> Word
init id substring =
  { id = id
  , substring = substring
  , cache =
      case Chord.fromCode substring.s of
        Nothing ->
          Nothing
        Just chord ->
          Just
            { chord = chord
            , codeName = Name.code chord
            }
  }

update : Maybe Substring -> Maybe Word -> Maybe (Maybe Word)
update mSubstring mWord =
  case ( mSubstring, mWord ) of
    ( Nothing, Nothing ) ->
      Just Nothing
    ( Just substring, Just word ) ->
      if substring.s == word.substring.s then
        Just (Just { word | substring = substring })
      else
        Nothing
    _ ->
      Nothing

highlight : Int -> Word -> Maybe Highlight
highlight key word =
  case word.cache of
    Nothing ->
      if word.substring.s == "_" then
        Just (Highlight "#808080" "#ffffff" word.substring)
      else
        Nothing
    Just cache ->
      if word.substring.s == cache.codeName then
        Just
          { fg = Colour.fg cache.chord
          , bg = Colour.swatchBg key cache.chord
          , substring = word.substring
          }
      else
        Nothing

meaning : Word -> Maybe (Maybe IdChord)
meaning word =
  case word.cache of
    Nothing ->
      if word.substring.s == "_" then
        Just Nothing
      else
        Nothing
    Just cache ->
      if word.substring.s == cache.codeName then
        Just (Just (IdChord word.id cache.chord))
      else
        Nothing

suggestion : Int -> Word -> Maybe (List Swatch, Substring)
suggestion key word =
  case word.cache of
    Nothing ->
      Nothing
    Just cache ->
      if word.substring.s == cache.codeName then
        Nothing
      else
        Just
          ( [ { fg = Colour.fg cache.chord
              , bg = Colour.swatchBg key cache.chord
              , s = cache.codeName
              }
            ]
          , word.substring
          )

mapChord : (Chord -> Chord) -> String -> String
mapChord f string =
  case Chord.fromCode string of
    Nothing ->
      string
    Just chord ->
      if Name.code chord == string then
        Name.code (f chord)
      else
        string
