module ChordsInKey exposing (view)

import Chord exposing (Chord)
import IdChord
import PlayStatus exposing (PlayStatus)
import Scale exposing (Scale)
import Storage exposing (Storage)

import Html exposing (Html, span, text, sup)
import Html.Attributes exposing (style)

view : String -> Storage -> Scale -> PlayStatus -> Html IdChord.Msg
view
  gridArea
  { harmonicMinor, extendedChords, addedToneChords }
  scale
  playStatus =
  let row = viewRow scale playStatus 0 in
    span
      [ style
          [ ( "grid-area", gridArea )
          , ( "justify-self", "start" )
          , ( "display", "grid" )
          , ( "position", "relative" )
          , ( "grid-template-rows", "auto" )
          , ( "grid-auto-rows", "75px" )
          , ( "grid-template-columns", "auto" )
          , ( "grid-auto-columns", "75px" )
          , ( "grid-row-gap", "5px" )
          , ( "grid-column-gap", "5px" )
          , ( "justify-items", "center" )
          , ( "font-size", "150%" )
          ]
      ]
      ( List.concat
          [ List.indexedMap
              (viewDegree << (+) 1)
              ( if scale.minor then
                  [ "i", "iio", "III", "iv"
                  , if harmonicMinor then "V" else "v"
                  , "VI", "VII"
                  ]
                else
                  [ "I", "ii"
                  , if harmonicMinor then "III" else "iii"
                  , "IV", "V", "vi", "viio"
                  ]
              )
          , [ viewCategory 1 "Triad" ]
          , if harmonicMinor then
              row 1 "C Dm E F G Am Bo"
            else
              row 1 "C Dm Em F G Am Bo"
          , [ viewCategory 2 "7th" ]
          , if harmonicMinor then
              row 2 "CM7 Dm7 E7 FM7 G7 AmM7 Bo7"
            else
              row 2 "CM7 Dm7 Em7 FM7 G7 Am7 B0"
          , if extendedChords || addedToneChords then
              List.concat
                [ [ viewCategory 3 "9th" ]
                , if extendedChords && harmonicMinor then
                    row 3 "CM9 Dm9 E7b9 FM9 G9 Am9"
                  else if extendedChords then
                    row 3 "CM9 Dm9 FM9 G9 Am9"
                  else
                    []
                , if addedToneChords then
                    row
                      (if extendedChords then 4 else 3)
                      "Cadd9 Dmadd9 Fadd9 Gadd9 Amadd9"
                  else
                    []
                , [ viewCategory
                      (if extendedChords && addedToneChords then 5 else 4)
                      "13th"
                  ]
                , if extendedChords then
                    row
                      (if addedToneChords then 5 else 4)
                      "CM13 Dm13 FM13 G13"
                  else
                    []
                , if addedToneChords then
                    row
                      (if extendedChords then 6 else 4)
                      "C6 Dm6 F6 G6"
                  else
                    []
                ]
            else
              []
          ]
      )

viewRow :
  Scale -> PlayStatus -> Int -> Int -> String -> List (Html IdChord.Msg)
viewRow scale playStatus sharpCount row code =
  let
    codes = String.split " " code
  in let
    chords = List.filterMap Chord.fromCode codes
  in let
    sortedChords =
      List.sortBy (degree sharpCount scale.minor) chords
  in
    List.map
      (viewChord scale playStatus sharpCount row)
      sortedChords

viewChord :
  Scale -> PlayStatus -> Int -> Int -> Chord -> Html IdChord.Msg
viewChord scale playStatus sharpCount row chord =
  let
    column =
      degree sharpCount scale.minor chord + 1
  in let
    idChord =
      case
        IdChord.fromChord
          (Chord.transpose scale.tonic chord)
      of
        Nothing ->
          Debug.crash
            ("ChordsInKey.viewChord: Unknown chord " ++ toString chord)
        Just something ->
          something
  in
    span
      [ style
          [ ( "grid-row-start", toString (row + 1) )
          , ( "grid-column-start", toString (column + 1) )
          , ( "grid-row-end", "span 1" )
          , ( "grid-column-end", "span 1" )
          , ( "align-self", "stretch" )
          , ( "justify-self", "stretch" )
          , ( "position", "relative" )
          ]
      ]
      (IdChord.view scale.tonic playStatus idChord)


degree : Int -> Bool -> Chord -> Int
degree sharpCount minor chord =
  let
    majorDegree =
      (chord.root * 7 + 6 - sharpCount) // 12
  in
    if minor then
      (majorDegree + 2) % 7
    else
      majorDegree

viewDegree : Int -> String -> Html msg
viewDegree x name =
  span
    [ style
        [ ( "grid-row", "1" )
        , ( "grid-column", toString (x + 1) )
        , ( "align-self", "baseline" )
        , ( "line-height", "initial" )
        , ( "display", "inline-block" )
        ]
    ]
    ( if String.endsWith "o" name then
        [ text (String.dropRight 1 name)
        , sup
            []
            [ text "o" ]
        ]
      else
        [ text name
        ]
    )

viewCategory : Int -> String -> Html msg
viewCategory y name =
  span
    [ style
        [ ( "grid-column", "1" )
        , ( "grid-row", toString (y + 1) )
        , ( "font-size", "initial" )
        , ( "display", "inline-block" )
        , ( "align-self", "center" )
        ]
    ]
    ( if String.endsWith "th" name then
        [ text (String.dropRight 2 name)
        , sup
            []
            [ text "th" ]
        ]
      else
        [ text name
        ]
    )

major : Int -> Chord
major = Chord [ 4, 7 ]

minor : Int -> Chord
minor = Chord [ 3, 7 ]

diminished : Int -> Chord
diminished = Chord [ 3, 6 ]

major7 : Int -> Chord
major7 = Chord [ 4, 7, 11 ]

minor7 : Int -> Chord
minor7 = Chord [ 3, 7, 10 ]

dominant7 : Int -> Chord
dominant7 = Chord [ 4, 7, 10 ]

halfDiminished7 : Int -> Chord
halfDiminished7 = Chord [ 3, 6, 10 ]

diminished7 : Int -> Chord
diminished7 = Chord [ 3, 6, 9 ]

sus4 : Int -> Chord
sus4 = Chord [ 5, 7 ]

dominant9 : Int -> Chord
dominant9 = Chord [ 4, 7, 10, 14 ]

major9 : Int -> Chord
major9 = Chord [ 4, 7, 11, 14 ]

add9 : Int -> Chord
add9 = Chord [ 4, 7, 14 ]

minorAdd9 : Int -> Chord
minorAdd9 = Chord [ 3, 7, 14 ]

minor9 : Int -> Chord
minor9 = Chord [ 3, 7, 10, 14 ]

dominant7Flat9 : Int -> Chord
dominant7Flat9 = Chord [ 4, 7, 10, 13 ]

dominant13 : Int -> Chord
dominant13 = Chord [ 4, 7, 10, 14, 21 ]

major13 : Int -> Chord
major13 = Chord [ 4, 7, 11, 14, 21 ]

minor13 : Int -> Chord
minor13 = Chord [ 3, 7, 10, 14, 21 ]

major6 : Int -> Chord
major6 = Chord [ 4, 7, 9 ]

minor6 : Int -> Chord
minor6 = Chord [ 3, 7, 9 ]
