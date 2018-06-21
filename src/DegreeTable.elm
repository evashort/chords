module DegreeTable exposing (view)

import Chord exposing (Chord)
import IdChord exposing (IdChord, PlayStatus)
import Scale exposing (Scale)

import Html exposing (Html, span, text, sup)
import Html.Attributes exposing (style)

view : String -> Scale -> PlayStatus -> Html IdChord.Msg
view gridArea scale playStatus =
  let
    key = Scale.key scale
  in let
    toIdChord =
      IdChord.fromChord << Chord.transpose scale.root
  in
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
          , ( "place-items", "center" )
          , ( "font-size", "150%" )
          ]
      ]
      ( List.concat
          [ List.indexedMap
              (viewDegree << (+) 1)
              ( if scale.minor then
                  [ "i", "iio", "III", "iv", "v", "VI", "VII" ]
                else
                  [ "I", "ii", "iii", "IV", "V", "vi", "viio" ]
              )
          , [ viewCategory 1 "Triad" ]
          , List.concat
              ( List.indexedMap
                  (IdChord.view key playStatus 1 << (+) 1)
                  ( List.map
                      toIdChord
                      ( if scale.minor then
                          minorTriads
                        else
                          majorTriads
                      )
                  )
              )
          , if scale.minor then
              List.concat
                [ IdChord.view key playStatus 2 3 (toIdChord (sus4 3))
                , IdChord.view key playStatus 2 5 (toIdChord (major 7))
                ]
            else
              List.concat
                [ IdChord.view key playStatus 2 1 (toIdChord (sus4 0))
                , IdChord.view key playStatus 2 3 (toIdChord (major 4))
                ]
          , [ viewCategory 3 "7th" ]
          , List.concat
              ( List.indexedMap
                  (IdChord.view key playStatus 3 << (+) 1)
                  ( List.map
                      toIdChord
                      ( if scale.minor then
                          minorSevenths
                        else
                          majorSevenths
                      )
                  )
              )
          , if scale.minor then
              List.concat
                [ IdChord.view key playStatus 4 2 (toIdChord (diminished7 2))
                , IdChord.view key playStatus 4 5 (toIdChord (dominant7 7))
                ]
            else
              List.concat
                [ IdChord.view key playStatus 4 3 (toIdChord (dominant7 4))
                , IdChord.view key playStatus 4 7 (toIdChord (diminished7 11))
                ]
          ]
      )

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

majorTriads : List Chord
majorTriads =
  [ major 0, minor 2, minor 4, major 5
  , major 7, minor 9, diminished 11
  ]

minorTriads : List Chord
minorTriads =
  [ minor 0, diminished 2, major 3, minor 5
  , minor 7, major 8, major 10
  ]

majorSevenths : List Chord
majorSevenths =
  [ major7 0, minor7 2, minor7 4, major7 5
  , dominant7 7, minor7 9, halfDiminished7 11
  ]

minorSevenths : List Chord
minorSevenths =
  [ minor7 0, halfDiminished7 2, major7 3, minor7 5
  , minor7 7, major7 8, dominant7 10
  ]

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
