module Chord exposing (Chord, fromRawName, arpeggio, view, fg, bg)

import Flavor exposing (Flavor)
import StaffMap exposing (StaffMap)

import Html exposing (Html)

type alias Chord = List Int

fromRawName : String -> Maybe Chord
fromRawName name =
  case name of
    "C" -> Just [ 48, 52, 55 ]
    "Dm" -> Just [ 50, 53, 57 ]
    "Em" -> Just [ 52, 55, 59 ]
    "F" -> Just [ 53, 57, 60 ]
    "G" -> Just [ 55, 59, 62 ]
    "Am" -> Just [ 57, 60, 64 ]
    "Bo" -> Just [ 59, 62, 65 ]
    _ -> Nothing

fg : Chord -> String
fg = .fg << Tuple.second << Flavor.get << intervals

bg : Chord -> String
bg chord =
  let
    ( i, flavor ) = Flavor.get (intervals chord)
  in
    nth3 (get i chord) flavor.bg

nth3 : Int -> ( a, a, a ) -> a
nth3 i ( x, y, z ) =
  case i % 3 of
    0 -> x
    1 -> y
    _ -> z

arpeggio : Chord -> List Int
arpeggio chord =
  let
    indices =
      case List.length chord of
        3 -> [ 3, 2, 1, 2, 0, 2, 1, 2 ]
        4 -> [ 4, 3, 2, 1, 0, 1, 2, 3 ]
        _ -> List.range 0 (List.length chord - 1)
  in
    multiGet indices chord

view : Chord -> List (Html msg)
view chord =
  let
    ( i, flavor ) = Flavor.get (intervals chord)
  in let
    namesake = get i chord
  in let
    staffNamesake = StaffMap.get namesake flavor.staffMap
  in let
    staffChord = List.map ((+) staffNamesake) flavor.staffOffsets
  in let
    root = get 0 chord
  in
    [ Html.text
        ( String.concat
            [ letter staffNamesake
            , prettyAccidental (namesake - get staffNamesake cScale)
            , flavor.prettyName
            ]
        )
    , Html.sup [] [ Html.text flavor.superscript ]
    ] ++
      ( case ( i, getOctaveName root ) of
          ( 0, "" ) -> []
          ( _, octaveName ) ->
            [ Html.text
                ( let
                    staffRoot = StaffMap.get -i staffChord
                  in
                    String.concat
                      [ "/"
                      , letter staffRoot
                      , prettyAccidental (root - get staffRoot cScale)
                      , octaveName
                      ]
                )
            ]
      )

intervals : Chord -> List Int
intervals chord =
  List.map2 (-) (invert 1 chord) chord

invert : Int -> Chord -> Chord
invert n chord =
  multiGet (List.range n (n + List.length chord - 1)) chord

multiGet : List Int -> Chord -> List Int
multiGet ns chord =
  List.map ((flip get) chord) ns

get : Int -> Chord -> Int
get n chord =
  let
    i = n % List.length chord
  in let
    octave = (n - i) // List.length chord
  in
    case List.drop i chord of
      pitch :: _ -> 12 * octave + pitch
      [] -> 0

cScale : Chord
cScale = [ 0, 2, 4, 5, 7, 9, 11 ]

letter : Int -> String
letter staffRow =
  let i = staffRow % 7 in
    String.slice i (i + 1) "CDEFGAB"

prettyAccidental : Int -> String
prettyAccidental accidental =
  if accidental < 0 then
    tally -accidental "♭" "𝄫"
  else
    tally accidental "♯" "𝄪"

tally : Int -> String -> String -> String
tally n one two =
  String.repeat (n % 2) one ++ String.repeat (n // 2) two

getOctaveName : Int -> String
getOctaveName pitch =
  case (pitch - pitch % 12) // 12 - 2 of
    2 -> ""
    octave -> toString octave
