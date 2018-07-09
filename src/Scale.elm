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
  , code = code
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
          in let
            tonic =
              if minor then
                (namesake + 3) % 12
              else
                namesake
          in
            Just (Scale minor tonic)
    _ ->
      Nothing

regex : Regex
regex = Regex.regex "^([A-Ga-g][b#♭♯]?)(.*)"

flavors : Dict String Bool
flavors =
  Dict.fromList
    [ ( "", False )
    , ( "m", True )
    ]

code : Scale -> String
code scale =
  if scale.minor then
    Pitch.code 3 ((scale.tonic - 3) % 12) ++ "m"
  else
    Pitch.code 0 scale.tonic
