module CachedChord exposing
  (CachedChord, fg, bg, borderOpacity, shineOpacity, view, fromChord)

import Chord exposing (Chord)
import Flavor exposing (Flavor)
import StaffMap exposing (StaffMap)

import Html exposing (Html, text, sup, sub)

type alias CachedChord =
  { chord : Chord
  , i : Int
  , flavor : Flavor
  , staffMap : StaffMap
  , codeName : String
  , prettyNamesake : String
  , prettyRoot : String
  }

fg : CachedChord -> String
fg x =
  x.flavor.fg

bg : Int -> CachedChord -> String
bg key x =
  let ( a, b, c ) = x.flavor.bg in
    case (Chord.get x.chord x.i - key) % 3 of
      0 -> a
      1 -> b
      _ -> c

borderOpacity : CachedChord -> String
borderOpacity x =
  if x.flavor.fg == "#ffffff" then "0.8" else "0.3"

shineOpacity : CachedChord -> String
shineOpacity x =
  if x.flavor.fg == "#ffffff" then "0.6" else "0.7"

view : CachedChord -> List (Html msg)
view x =
  case ( x.flavor.superscript, x.prettyRoot ) of
    ( "", "" ) ->
      [ text (x.prettyNamesake ++ x.flavor.prettyName) ]
    ( _, "" ) ->
      [ text (x.prettyNamesake ++ x.flavor.prettyName)
      , sup [] [ text x.flavor.superscript ]
      ]
    ( "", _ ) ->
      [ text (x.prettyNamesake ++ x.flavor.prettyName ++ "/")
      , sub [] [ text x.prettyRoot ]
      ]
    _ ->
      [ text (x.prettyNamesake ++ x.flavor.prettyName)
      , sup [] [ text x.flavor.superscript ]
      , text "⁄"
      , sub [] [ text x.prettyRoot ]
      ]

fromChord : Chord -> CachedChord
fromChord chord =
  let
    ( i, maybeFlavor ) = Flavor.get (Chord.intervals chord)
  in let
    flavor = Maybe.withDefault Flavor.errorFlavor maybeFlavor
  in let
    staffNamesake = StaffMap.get flavor.staffMap (Chord.get chord i)
  in let
    staffMap =
      StaffMap.invert
        -i
        (List.map ((+) staffNamesake) flavor.staffOffsets)
  in let
    ( codeNamesake, prettyNamesake ) = getNoteNames chord staffMap i ""
  in let
    root = Chord.get chord 0
  in let
    octaveName =
      case (root - root % 12) // 12 - 2 of
        2 -> ""
        octave -> toString octave
  in let
    ( codeRoot, prettyRoot ) =
      case ( octaveName, i ) of
        ( "", 0 ) -> ( "", "" )
        _ -> getNoteNames chord staffMap 0 octaveName
  in
    { chord = chord
    , i = i
    , flavor = flavor
    , staffMap = staffMap
    , codeName =
        String.concat
          [ codeNamesake
          , flavor.codeName
          , if codeRoot == "" then "" else "/" ++ codeRoot
          ]
    , prettyNamesake = prettyNamesake
    , prettyRoot = prettyRoot
    }

getNoteNames : Chord -> StaffMap -> Int -> String -> ( String, String )
getNoteNames chord staffMap i octaveName =
  let
    staffRow = StaffMap.get staffMap i
  in let
    letterIndex = staffRow % 7
  in let
    letter = String.slice letterIndex (letterIndex + 1) "CDEFGAB"
  in let
    letterPitch = Chord.get [ 0, 2, 4, 5, 7, 9, 11 ] staffRow
  in let
    accidental = Chord.get chord i - letterPitch
  in
    if accidental < 0 then
      ( letter ++ String.repeat -accidental "b" ++ octaveName
      , letter ++ tally -accidental "♭" "𝄫" ++ octaveName
      )
    else
      ( letter ++ tally accidental "#" "x" ++ octaveName
      , letter ++ tally accidental "♯" "𝄪" ++ octaveName
      )

tally : Int -> String -> String -> String
tally n one two =
  String.repeat (n % 2) one ++ String.repeat (n // 2) two
