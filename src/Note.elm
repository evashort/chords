module Note exposing (Note, fromPitch, sumCost, rawName, prettyName)

import Scale exposing (Scale)

type alias Note =
  { staffRow : Int
  , accidental : Int
  }

fromPitch : Int -> Int -> Note
fromPitch staffRow pitch =
  { staffRow = staffRow
  , accidental = pitch - Scale.get staffRow Scale.cMajor
  }

sumCost : List Note -> Int
sumCost notes =
  List.sum (List.map (abs << .accidental) notes)

rawName : Note -> String
rawName note =
  getLetter note ++
    ( if note.accidental < 0 then
        String.repeat -note.accidental "b"
      else
        tally note.accidental "#" "x"
    )

prettyName : Note -> String
prettyName note =
  getLetter note ++
    ( if note.accidental < 0 then
        tally -note.accidental "♭" "𝄫"
      else
        tally note.accidental "♯" "𝄪"
    )

getLetter : Note -> String
getLetter note =
  case note.staffRow % 7 of
    0 -> "C"
    1 -> "D"
    2 -> "E"
    3 -> "F"
    4 -> "G"
    5 -> "A"
    _ -> "B"

tally : Int -> String -> String -> String
tally n one two =
  String.repeat (n % 2) one ++ String.repeat (n // 2) two
