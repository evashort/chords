module Note exposing (Note, init)

import Chord

type alias Note =
  { codeName : String
  , prettyName : String
  }

init : Int -> Int -> Note
init pitch staffRow =
  let
    letterIndex = staffRow % 7
  in let
    letter = String.slice letterIndex (letterIndex + 1) "CDEFGAB"
  in let
    letterPitch = Chord.get [ 0, 2, 4, 5, 7, 9, 11 ] staffRow
  in let
    accidental = pitch - letterPitch
  in
    if accidental < 0 then
      { codeName = letter ++ String.repeat -accidental "b"
      , prettyName = letter ++ tally -accidental "♭" "𝄫"
      }
    else
      { codeName = letter ++ tally accidental "#" "x"
      , prettyName = letter ++ tally accidental "♯" "𝄪"
      }

tally : Int -> String -> String -> String
tally n one two =
  String.repeat (n % 2) one ++ String.repeat (n // 2) two
