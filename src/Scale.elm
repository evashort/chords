module Scale exposing (Scale, parse, pair)

import Flag exposing (Flag, Rule)
import Substring exposing (Substring)

import Dict exposing (Dict)

type alias Scale =
  { root : Int
  , minor : Bool
  }

parse : List Substring -> Scale
parse = Flag.parse flag

rule : Rule
rule = Flag.rule flag

flag : Flag
flag =
  { name = "scale"
  , fromString = fromString
  , code = code
  , default =
      { root = 0
      , minor = False
      }
  }

fromString : String -> Maybe Scale
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
        case Dict.get flavors flavorName of
          Nothing ->
            Nothing
          Just minor ->
            Just
              { root = (whiteRoot + accidentalOffset) % 12
              , minor = minor
              }

flavors : Dict String Bool
flavors =
  Dict.fromList
    [ ( "", False )
    , ( "m", True )
    ]

code : Scale -> String
code scale =
  let
    sharpCount = if scale.minor then 3 else 0
  in let
    letterIndex = (scale.root * 7 + 6 - sharpCount) // 12
  in let
    letterPitch = (letterIndex * 12 + 5) // 7
  in
    String.concat
      [ String.slice letterIndex (letterIndex + 1) "CDEFGAB"
      , case scale.root - letterPitch of
          -1 -> "b"
          1 -> "#"
          _ -> ""
      , if scale.minor then "m" else ""
      ]
