module LowestNote exposing (flag, view)

import Flag exposing (Flag)
import Pitch
import Submatches exposing (submatches)

import Regex exposing (Regex)

flag : Flag Int
flag =
  { key = "octave"
  , fromCode = fromCode
  , code = code
  , default = 45
  }

fromCode : String -> Maybe Int
fromCode code =
  case submatches regex code of
    [ _, Just "-" ] ->
      Nothing
    [ Just pitchCode, Just octaveCode ] ->
      case String.toInt octaveCode of
        Err _ ->
          Nothing
        Ok octave ->
          Just (12 + 12 * octave + Pitch.fromCode pitchCode)
    _ ->
      Nothing

regex : Regex
regex = Regex.regex "^([A-Ga-g][b#♭♯]?)(.*)"

code : Int -> String
code lowestNote =
  let
    pitch = lowestNote % 12
  in let
    octave = (lowestNote - pitch) // 12 - 1
  in
    Pitch.code 0 pitch ++ toString octave

view : Int -> String
view lowestNote =
  let
    pitch = lowestNote % 12
  in let
    octave = (lowestNote - pitch) // 12 - 1
  in
    Pitch.view 0 pitch ++ toString octave
