module LowestNote exposing (flag)

import Flag exposing (Flag)
import Pitch
import Submatches exposing (submatches)

import Regex exposing (Regex)

flag : Flag
flag =
  { key = "octave"
  , fromCode = fromCode
  , code = code
  , default = 48
  }

fromCode : String -> Maybe Int
fromCode code =
  case submatches regex code of
    [ Just pitchCode, Just octaveCode ] ->
      case String.parseInt octaveCode of
        Err _ ->
          Nothing
        Ok octave ->
          Just (24 + 12 * octave + Pitch.fromCode pitchCode)
    _ ->
      Nothing

regex : Regex
regex = "^([A-Ga-g][b#♭♯]?)(.*)"

code : Int -> String
code lowestNote =
  let
    pitch = lowestNote % 12
  in let
    octave = (lowestNote - pitch) // 12 - 2
  in
    Pitch.code pitch ++ toString octave
