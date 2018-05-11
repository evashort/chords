module Scale exposing (Scale, rule, parse, insert)

import Flag exposing (Flag, Rule)
import Pitch
import Replacement exposing (Replacement)
import Submatches exposing (submatches)
import Substring exposing (Substring)

import Dict exposing (Dict)
import Regex exposing (Regex)

type alias Scale =
  { minor : Bool
  , root : Int
  }

rule : Rule
rule = Flag.rule flag

parse : List Substring -> Scale
parse = Flag.parse flag

insert : Scale -> List Substring -> Maybe Replacement
insert = Flag.insert flag

flag : Flag
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
      case Dict.get flavors flavorCode of
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
regex = "^([A-Ga-g][b#♭♯]?)(.*)"

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
