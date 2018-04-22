module Word exposing
  (Word, init, update, view, meaning, isNewline, suggestion, transpose)

import Chord exposing (Chord)
import Colour
import Highlight exposing (Highlight)
import IdChord exposing (IdChord)
import Name
import Replacement exposing (Replacement)
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
      case Chord.fromString substring.s of
        Nothing ->
          Nothing
        Just chord ->
          Just
            { chord = chord
            , codeName = Name.code chord
            }
  }

update : Substring -> Word -> Maybe Word
update substring word =
  if substring.s == word.substring.s then
    Just { word | substring = substring }
  else
    Nothing

view : Int -> Word -> Maybe Highlight
view key word =
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
          , bg = Colour.bg key cache.chord
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

isNewline : Word -> Bool
isNewline word =
  word.substring.s == "\n"

suggestion : Int -> Word -> Maybe ( List Swatch, Substring )
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
              , bg = Colour.bg key cache.chord
              , s = cache.codeName
              }
            ]
          , word.substring
          )

transpose : Int -> Word -> Maybe Replacement
transpose offset word =
  case word.cache of
    Nothing ->
      Nothing
    Just cache ->
      if word.substring.s == cache.codeName then
        Just
          { old = word.substring
          , new =
              Name.code
                { flavor = cache.chord.flavor
                , root = (cache.chord.root + offset) % 12
                }
          }
      else
        Nothing
