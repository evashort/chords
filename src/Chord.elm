module Chord exposing (Chord, fromCode)

import Submatches exposing (submatches)

import Dict exposing (Dict)
import Regex exposing (Regex)

type alias Chord =
  { flavor : List Int
  , root : Int
  }

fromCode : String -> Maybe Chord
fromCode code =
  case submatches regex code of
    [ Just rootCode, Just flavorCode ] ->
      case Dict.get flavors flavorCode of
        Nothing ->
          Nothing
        Just flavor ->
          Just
            { flavor = flavor
            , root = Pitch.fromCode rootCode
            }
    _ ->
      Nothing

regex : Regex
regex = "^([A-Ga-g][b#♭♯]?)(.*)"

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
