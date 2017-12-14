module NoteParser exposing (parseAtStart)

import Regex exposing (Regex, HowMany(..), Match)

parseAtStart : String -> Maybe ( Int, String )
parseAtStart code =
  case
    Regex.find
      (AtMost 1)
      (Regex.regex "^[A-Ga-g][♭b#♯x*ˣ×]*")
      (normalizeTwoCharAccidentals code)
  of
    [] ->
      Nothing
    match :: _ ->
      let
        letterPitch =
          case String.toUpper (String.left 1 match.match) of
            "C" -> 0
            "D" -> 2
            "E" -> 4
            "F" -> 5
            "G" -> 7
            "A" -> 9
            "B" -> 11
            _ -> 0
      in let
        accidentalValue =
          List.sum
            ( List.map
                getAccidentalCharValue
                (String.toList (String.dropLeft 1 match.match))
            )
      in
        Just
          ( (letterPitch + accidentalValue) % 12
          , String.dropLeft (String.length match.match) code
          )

normalizeTwoCharAccidentals : String -> String
normalizeTwoCharAccidentals code =
  Regex.replace All (Regex.regex "𝄫|𝄪") normalizeTwoCharAccidental code

normalizeTwoCharAccidental : Match -> String
normalizeTwoCharAccidental match =
  case match.match of
    "𝄫" -> "bb"
    "𝄪" -> "##"
    _ -> match.match

getAccidentalCharValue : Char -> Int
getAccidentalCharValue char =
  case char of
    '♭' -> -1
    'b' -> -1
    '#' -> 1
    '♯' -> 1
    'x' -> 2
    '*' -> 2
    'ˣ' -> 2
    '×' -> 2
    _ -> 0
