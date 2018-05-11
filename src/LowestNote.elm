module LowestNote exposing (rule, parse, insert)

import Flag exposing (Flag, Rule)
import Pitch
import Replacement exposing (Replacement)
import Submatches exposing (submatches)
import Substring exposing (Substring)

import Regex exposing (Regex)

rule : Rule
rule = Flag.rule flag

parse : List Substring -> Int
parse = Flag.parse flag

insert : Int -> List Substring -> Maybe Replacement
insert = Flag.insert flag

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
