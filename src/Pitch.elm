module Pitch exposing (fromCode, code, view)

fromCode : String -> Int
fromCode code =
  let
    letterPitch =
      case String.toUpper (String.left 1 code) of
        "C" -> 0
        "D" -> 2
        "E" -> 4
        "F" -> 5
        "G" -> 7
        "A" -> 9
        "B" -> 11
        x ->
          Debug.crash ("Pitch.fromCode: Unknown letter: " ++ x)
  in let
    accidentalOffset =
      case String.dropLeft 1 code of
        "" -> 0
        "b" -> -1
        "#" -> 1
        "♭" -> -1
        "♯" -> 1
        x ->
          Debug.crash ("Pitch.fromCode: Unknown accidental: " ++ x)
  in
    (letterPitch + accidentalOffset) % 12

-- sharpCount = how many of the 5 black keys are expressed as sharps instead
-- of flats. sharps are added in the order F# C# G# D# A#

code : Int -> Int -> String
code sharpCount pitch =
  let
    letterIndex = (pitch * 7 + 6 - sharpCount) // 12
  in let
    letterPitch = (letterIndex * 12 + 5) // 7
  in let
    letter = String.slice letterIndex (letterIndex + 1) "CDEFGAB"
  in let
    accidental =
      case pitch - letterPitch of
        0 -> ""
        (-1) -> "b"
        1 -> "#"
        _ ->
          Debug.crash ("Pitch.code: Pitch caused error: " ++ toString pitch)
  in
    letter ++ accidental

view : Int -> Int -> String
view sharpCount pitch =
  let
    staffRow = (pitch * 7 + 6 - sharpCount) // 12
  in let
    letterPitch = (staffRow * 12 + 5) // 7
  in let
    accidental = tally (pitch - letterPitch)
    letterIndex = staffRow % 7
  in let
    letter =
      String.slice letterIndex (letterIndex + 1) "CDEFGAB"
  in
    letter ++ accidental

tally : Int -> String
tally x =
  if x > 0 then
    String.repeat (x % 2) "♯" ++ String.repeat (x // 2) "𝄪"
  else
    String.repeat (x % 2) "♭" ++ String.repeat (-x // 2) "𝄫"
