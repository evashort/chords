module Chord exposing (Chord, view, fg, bg, invert, indexOf, getSquared)

import Flavor exposing (Flavor)
import StaffMap exposing (StaffMap)

import Array exposing (Array)
import Html exposing (Html)

type alias Chord = List Int

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
  in let
    a =
      String.concat
        [ letter staffNamesake
        , prettyAccidental (namesake - get staffNamesake cScale)
        , flavor.prettyName
        ]
  in let
    b = flavor.superscript
  in let
    c =
      case ( i, getOctaveName root ) of
        ( 0, "" ) -> ""
        ( _, octaveName ) ->
          let staffRoot = StaffMap.get -i staffChord in
            String.concat
              [ letter staffRoot
              , prettyAccidental (root - get staffRoot cScale)
              , octaveName
              ]
  in
    case ( b, c ) of
      ( "", "" ) ->
        [ Html.text a ]
      ( _, "" ) ->
        [ Html.text a, Html.sup [] [ Html.text b ] ]
      ( "", _ ) ->
        [ Html.text (a ++ "/"), Html.sub [] [ Html.text c ] ]
      _ ->
        [ Html.text a
        , Html.sup [] [ Html.text b ]
        , Html.text "⁄"
        , Html.sub [] [ Html.text c ]
        ]

intervals : Chord -> List Int
intervals chord =
  List.map2 (-) (invert 1 chord) chord

invert : Int -> Chord -> Chord
invert n chord =
  multiGet (List.range n (n + List.length chord - 1)) chord

getSquared : List (List Int) -> Chord -> List (List Int)
getSquared nss chord =
  List.map ((flip multiGet) chord) nss

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

indexOf : Int -> Chord -> List Int
indexOf pitch =
  zeros << List.map ((+) -pitch)

zeros : Chord -> List Int
zeros chord =
  List.filterMap
    identity
    (List.indexedMap (zerosHelp (List.length chord)) chord)

zerosHelp : Int -> Int -> Int -> Maybe Int
zerosHelp n i pitch =
  if pitch % 12 == 0 then
    Just (i - n * (pitch // 12))
  else
    Nothing

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
