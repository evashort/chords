module Name exposing (code, codeExtended, view, sharpCount)

import Chord exposing (Chord)
import Pitch

import Dict exposing (Dict)
import Html exposing (Html, text, sup)

code : Chord -> String
code chord =
  case Dict.get chord.flavor schemes of
    Nothing ->
      "unknown"
    Just scheme ->
      Pitch.toCode scheme.sharpCount chord.root ++ scheme.code

codeExtended : Chord -> String
codeExtended chord =
  case Dict.get chord.flavor schemes of
    Just scheme ->
      Pitch.toCode scheme.sharpCount chord.root ++ scheme.code
    Nothing ->
      String.join
        " "
        ( List.map
            (String.fromInt << (+) chord.root)
            (0 :: chord.flavor)
        )

view : Chord -> List (Html msg)
view chord =
  let
    scheme =
      Maybe.withDefault unknown (Dict.get chord.flavor schemes)
  in
  let
    normal =
      Pitch.view scheme.sharpCount chord.root ++ scheme.normal
  in
    if scheme.superscript == "" then
      [ text normal ]
    else
      [ text normal, sup [] [ text scheme.superscript ] ]

sharpCount : List Int -> Int
sharpCount flavor =
  case Dict.get flavor schemes of
    Nothing ->
      Debug.todo
        ("Name.sharpCount: Unknown flavor: " ++ Debug.toString flavor)
    Just scheme ->
      scheme.sharpCount

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
  Chord.dict
    [ ( "", Scheme "" "" "" 0 )
    , ( "m", Scheme "m" "m" "" 3 )
    , ( "o", Scheme "o" "" "o" 5 )
    , ( "7", Scheme "7" "" "7" 1 )
    , ( "M7", Scheme "M7" "" "M7" 0 )
    , ( "m7", Scheme "m7" "m" "7" 3 )
    , ( "0", Scheme "0" "" "ø" 5 )
    , ( "o7", Scheme "o7" "" "o7" 5 )
    , ( "mM7", Scheme "mM7" "m" "M7" 3 )
    , ( "9", Scheme "9" "" "9" 1 )
    , ( "M9", Scheme "M9" "" "M9" 0 )
    , ( "m9", Scheme "m9" "m" "9" 3 )
    , ( "7b9", Scheme "7b9" "" "7♭9" 1 )
    , ( "6/9", Scheme "6/9" "" "6/9" 1 )
    , ( "M7#11", Scheme "M7#11" "" "M7♯11" 0 )
    , ( "13", Scheme "13" "" "13" 1 )
    , ( "M13", Scheme "M13" "" "M13" 0 )
    , ( "add9", Scheme "add9" "" "add9" 0 )
    , ( "madd9", Scheme "madd9" "m" "add9" 3 )
    , ( "addb9", Scheme "addb9" "" "add♭9" 1 )
    , ( "add#11", Scheme "add#11" "" "add♯11" 0 )
    , ( "6", Scheme "6" "" "6" 0 )
    , ( "m6", Scheme "m6" "m" "6" 2 )
    , ( "+", Scheme "+" "+" "" 0 )
    , ( "sus4", Scheme "sus4" "" "sus4" 1 )
    , ( "sus2", Scheme "sus2" "" "sus2" 0 )
    ]

unknown : Scheme
unknown =
  Scheme "unknown" "unknown" "" 0
