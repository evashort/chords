module Flag exposing (Flag(..), parseKey, codeValue, getKey)

import NoteParser

type Flag
  = KeyFlag Int

parseKey : String -> Maybe Flag
parseKey code =
  case NoteParser.parseAtStart code of
    Nothing ->
      Nothing
    Just ( pitch, flavor ) ->
      let
        offset =
          case flavor of
            "" -> 0
            "M" -> 0
            "maj" -> 0
            "major" -> 0
            " major" -> 0
            "-" -> 3
            "m" -> 3
            "min" -> 3
            "minor" -> 3
            " minor" -> 3
            _ -> -1
      in
        if offset == -1 then
          Nothing
        else
          Just (KeyFlag ((pitch + offset) % 12))

codeValue : Flag -> String
codeValue flag =
  case flag of
    KeyFlag key ->
      case key of
        0 -> "C"
        1 -> "Db"
        2 -> "D"
        3 -> "Eb"
        4 -> "E"
        5 -> "F"
        6 -> "Gb"
        7 -> "G"
        8 -> "Ab"
        9 -> "A"
        10 -> "Bb"
        11 -> "B"
        _ -> "error"

getKey : Flag -> Maybe Int
getKey flag =
  case flag of
    KeyFlag key -> Just key
