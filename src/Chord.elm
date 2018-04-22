module Chord exposing (Chord, fromString)

import Dict exposing (Dict)

type alias Chord =
  { flavor : List Int
  , root : Int
  }

fromString : String -> Maybe Chord
fromString s =
  let
    whiteRoot =
      case String.toUpper (String.left 1 s) of
        "C" -> 0
        "D" -> 2
        "E" -> 4
        "F" -> 5
        "G" -> 7
        "A" -> 9
        "B" -> 11
        _ -> -1
  in
    if whiteRoot == -1 then
      Nothing
    else
      let
        accidentalOffset =
          case String.slice 1 2 s of
            "b" -> -1
            "#" -> 1
            _ -> 0
      in let
        flavorName =
          String.dropLeft (if accidentalOffset == 0 then 1 else 2) s
      in
        case Dict.get flavorName flavors of
          Nothing ->
            Nothing
          Just flavor ->
            Just
              { flavor = flavor
              , root = (whiteRoot + accidentalOffset) % 12
              }

flavors : Dict String (List Int)
flavors =
  Dict.fromList
    [ ( "", [ 4, 7 ] )
    , ( "m", [ 3, 7 ] )
    , ( "o", [ 3, 6 ] )
    , ( "+", [ 4, 8 ] )
    , ( "sus4", [ 5, 7 ] )
    , ( "sus2", [ 2, 7 ] )
    , ( "7", [ 4, 7, 10 ] )
    , ( "M7", [ 4, 7, 11 ] )
    , ( "m7", [ 3, 7, 10 ] )
    , ( "6", [ 4, 7, 9 ] )
    , ( "0", [ 3, 6, 10 ] )
    , ( "m6", [ 3, 7, 9 ] )
    , ( "o7", [ 3, 6, 9 ] )
    , ( "mM7", [ 3, 7, 11 ] )
    ]
