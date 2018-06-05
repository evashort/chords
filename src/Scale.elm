module Scale exposing (Scale, flag)

import Flag exposing (Flag)
import Pitch
import Submatches exposing (submatches)

import Dict exposing (Dict)
import Regex exposing (Regex)

type alias Scale =
  { minor : Bool
  , root : Int
  }

flag : Flag Scale
flag =
  { key = "scale"
  , fromCode = fromCode
  , code = code
  , default =
      { minor = False
      , root = 0
      }
  }

fromCode : String -> Maybe Scale
fromCode code =
  case submatches regex code of
    [ Just rootCode, Just flavorCode ] ->
      case Dict.get flavorCode flavors of
        Nothing ->
          Nothing
        Just minor ->
          Just
            { minor = minor
            , root = Pitch.fromCode rootCode
            }
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
    Pitch.code 3 scale.root ++ "m"
  else
    Pitch.code 0 scale.root