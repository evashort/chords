module Pitch exposing (fromCode, toCode, view)

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
          Debug.todo ("Pitch.fromCode: Unknown letter: " ++ x)
    accidentalOffset =
      case String.dropLeft 1 code of
        "" -> 0
        "b" -> -1
        "#" -> 1
        "♭" -> -1
        "♯" -> 1
        x ->
          Debug.todo ("Pitch.fromCode: Unknown accidental: " ++ x)
  in
    modBy 12 (letterPitch + accidentalOffset)

-- sharpCount = how many of the 5 black keys are expressed as sharps instead
-- of flats. sharps are added in the order F# C# G# D# A#

toCode : Int -> Int -> String
toCode sharpCount pitch =
  let
    letterIndex = (pitch * 7 + 6 - sharpCount) // 12
  in
  let
    letterPitch = (letterIndex * 12 + 5) // 7
    letter =
      String.slice letterIndex (letterIndex + 1) "CDEFGAB"
    accidental =
      case pitch - letterPitch + 1 of -- elm can't handle negative cases
        1 -> ""
        0 -> "b"
        2 -> "#"
        _ ->
          Debug.todo
            ("Pitch.code: Pitch caused error: " ++ Debug.toString pitch)
  in
    letter ++ accidental

view : Int -> Int -> String
view sharpCount pitch =
  let
    staffRow = (pitch * 7 + 6 - sharpCount) // 12
  in
  let
    letterPitch = (staffRow * 12 + 5) // 7
    letterIndex = modBy 7 staffRow
  in
  let
    accidental = tally (pitch - letterPitch)
    letter =
      String.slice letterIndex (letterIndex + 1) "CDEFGAB"
  in
    letter ++ accidental

tally : Int -> String
tally x =
  if x > 0 then
    String.repeat (modBy 2 x) "♯" ++ String.repeat (x // 2) "𝄪"
  else
    String.repeat (modBy 2 x) "♭" ++ String.repeat (-x // 2) "𝄫"
