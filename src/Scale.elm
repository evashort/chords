module Scale exposing (Scale, flag)

import Flag exposing (Flag)
import Pitch
import Submatches exposing (submatches)

import Dict exposing (Dict)
import Regex exposing (Regex)

type alias Scale =
  { minor : Bool
  , tonic : Int
  }

flag : Flag Scale
flag =
  { key = "key"
  , fromCode = fromCode
  , toCode = toCode
  , default =
      { minor = False
      , tonic = 0
      }
  }

fromCode : String -> Maybe Scale
fromCode code =
  case submatches regex code of
    [ Just namesakeCode, Just flavorCode ] ->
      case Dict.get flavorCode flavors of
        Nothing ->
          Nothing
        Just minor ->
          let
            namesake = Pitch.fromCode namesakeCode
          in
          let
            tonic =
              if minor then
                modBy 12 (namesake + 3)
              else
                namesake
          in
            Just (Scale minor tonic)
    _ ->
      Nothing

regex : Regex
regex =
  Maybe.withDefault
    Regex.never
    (Regex.fromString "^([A-Ga-g][b#♭♯]?)(.*)")

flavors : Dict String Bool
flavors =
  Dict.fromList
    [ ( "", False )
    , ( "m", True )
    ]

toCode : Scale -> String
toCode scale =
  if scale.minor then
    Pitch.toCode 3 (modBy 12 (scale.tonic - 3)) ++ "m"
  else
    Pitch.toCode 0 scale.tonic
