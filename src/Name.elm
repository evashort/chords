module Name exposing (code, view)

import Chord exposing (Chord)

import Dict exposing (Dict)
import Html exposing (Html, text, sup)

code : Chord -> String
code chord =
  let
    scheme =
      Maybe.withDefault unknown (Dict.get chord.flavor schemes)
  in let
    letterIndex = (chord.root * 7 + 6 - scheme.sharpCount) // 12
  in let
    letterPitch = (letterIndex * 12 + 5) // 7
  in
    String.concat
      [ String.slice letterIndex (letterIndex + 1) "CDEFGAB"
      , case chord.root - letterPitch of
          -1 -> "b"
          1 -> "#"
          _ -> ""
      , scheme.code
      ]

view : Chord -> List (Html msg)
view chord =
  let
    scheme =
      Maybe.withDefault unknown (Dict.get chord.flavor schemes)
  in let
    letterIndex = (chord.root * 7 + 6 - scheme.sharpCount) // 12
  in let
    letterPitch = (letterIndex * 12 + 5) // 7
  in let
    normal =
      String.concat
        [ String.slice letterIndex (letterIndex + 1) "CDEFGAB"
        , case chord.root - letterPitch of
            -1 -> "♭"
            1 -> "♯"
            _ -> ""
        , scheme.normal
        ]
  in
    if scheme.superscript == "" then
      [ text normal ]
    else
      [ text normal, sup [] [ text scheme.superscript ] ]

type alias Scheme =
  { code : String
  , normal : String
  , superscript : String
  -- how many of the 5 black-key roots are expressed as sharps instead of
  -- flats. sharps are added in the order F# C# G# D# A#
  , sharpCount : Int
  }

schemes : Dict (List Int) Scheme
schemes =
  Dict.fromList
    [ ( [ 4, 7 ], Scheme "" "" "" 0 )
    , ( [ 3, 7 ], Scheme "m" "m" "" 3 )
    , ( [ 3, 6 ], Scheme "o" "" "o" 5 )
    , ( [ 4, 8 ], Scheme "+" "+" "" 0 )
    , ( [ 5, 7 ], Scheme "sus4" "" "sus4" 0 )
    , ( [ 2, 7 ], Scheme "sus2" "" "sus2" 0 )
    , ( [ 4, 7, 10 ], Scheme "7" "" "7" 1 )
    , ( [ 4, 7, 11 ], Scheme "M7" "" "M7" 0 )
    , ( [ 3, 7, 10 ], Scheme "m7" "m" "7" 3 )
    , ( [ 4, 7, 9 ], Scheme "6" "" "6" 0 )
    , ( [ 3, 6, 10 ], Scheme "0" "" "ø" 5 )
    , ( [ 3, 7, 9 ], Scheme "m6" "m" "6" 2 )
    , ( [ 3, 6, 9 ], Scheme "o7" "" "o7" 5 )
    , ( [ 3, 7, 11 ], Scheme "mM7" "m" "M7" 3 )
    ]

unknown : Scheme
unknown =
  Scheme "unknown" "unknown" "" 0
