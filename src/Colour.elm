module Colour exposing (borderOpacity, shineOpacity, fg, bg, swatchBg)

import Chord exposing (Chord)

import Dict exposing (Dict)

borderOpacity : Chord -> String
borderOpacity chord =
  if fg chord == "#ffffff" then "0.8" else "0.3"

shineOpacity : Chord -> String
shineOpacity chord =
  if fg chord == "#ffffff" then "0.6" else "0.7"

fg : Chord -> String
fg chord =
  case Dict.get chord.flavor schemes of
    Nothing ->
      black
    Just scheme ->
      scheme.fg

bg : Int -> Chord -> String
bg key chord =
  case Dict.get chord.flavor schemes of
    Nothing ->
      gray
    Just scheme ->
      case (chord.root - key) % 3 of
        0 -> scheme.c
        1 -> scheme.g
        _ -> scheme.f

swatchBg : Int -> Chord -> String
swatchBg key chord =
  case Dict.get chord.flavor swatchSchemes of
    Nothing ->
      swatchGray
    Just scheme ->
      case (chord.root - key) % 3 of
        0 -> scheme.c
        1 -> scheme.g
        _ -> scheme.f

type alias Scheme =
  { fg : String
  , f : String
  , c : String
  , g : String
  }

schemes : Dict (List Int) Scheme
schemes =
  Dict.fromList
    [ ( [ 4, 7 ], major )
    , ( [ 3, 7 ], minor )
    , ( [ 3, 6 ], diminished )
    , ( [ 4, 8 ], major )
    , ( [ 5, 7 ], major )
    , ( [ 2, 7 ], sus2 )
    , ( [ 4, 7, 10 ], dominant7 )
    , ( [ 4, 7, 11 ], major7 )
    , ( [ 3, 7, 10 ], minor7 )
    , ( [ 4, 7, 9 ], minor7 )
    , ( [ 3, 6, 10 ], minor6 )
    , ( [ 3, 7, 9 ], minor6 )
    , ( [ 3, 6, 9 ], diminished )
    , ( [ 3, 7, 11 ], minor )
    ]

major : Scheme
major =
  { fg = black
  , f = green
  , c = blue
  , g = red
  }

sus2 : Scheme
sus2 =
  { fg = black
  , f = blue
  , c = red
  , g = green
  }

minor : Scheme
minor =
  { fg = black
  , f = yellow
  , c = cyan
  , g = purple
  }

major7 : Scheme
major7 =
  { fg = black
  , f = mix green cyan
  , c = mix blue purple
  , g = mix red yellow
  }

minor7 : Scheme
minor7 =
  { fg = black
  , f = mix yellow green
  , c = mix cyan blue
  , g = mix purple red
  }

dominant7 : Scheme
dominant7 =
  { fg = white
  , f = darkGreen
  , c = darkBlue
  , g = darkRed
  }

minor6 : Scheme
minor6 =
  { fg = white
  , f = darkYellow
  , c = darkCyan
  , g = darkPurple
  }

diminished : Scheme
diminished =
  { fg = white
  , f = mix darkRed darkYellow
  , c = mix darkGreen darkCyan
  , g = mix darkBlue darkPurple
  }

mix : String -> String -> String
mix x y =
  String.concat
    [ "#"
    , toHexPair ((parseHexPair 1 x + parseHexPair 1 y) // 2)
    , toHexPair ((parseHexPair 3 x + parseHexPair 3 y) // 2)
    , toHexPair ((parseHexPair 5 x + parseHexPair 5 y) // 2)
    ]

toHexPair : Int -> String
toHexPair n =
  let
    ones = n % 16
  in let
    sixteens = n // 16
  in
    String.concat
      [ String.slice sixteens (sixteens + 1) "0123456789abcdef"
      , String.slice ones (ones + 1) "0123456789abcdef"
      ]

parseHexPair : Int -> String -> Int
parseHexPair offset s =
  16 * parseHexDigit (String.slice offset (offset + 1) s) +
    parseHexDigit (String.slice (offset + 1) (offset + 2) s)

parseHexDigit : String -> Int
parseHexDigit s =
  case s of
    "1" -> 1
    "2" -> 2
    "3" -> 3
    "4" -> 4
    "5" -> 5
    "6" -> 6
    "7" -> 7
    "8" -> 8
    "9" -> 9
    "a" -> 10
    "b" -> 11
    "c" -> 12
    "d" -> 13
    "e" -> 14
    "f" -> 15
    _ -> 0

black : String
black = "#000000"

white : String
white = "#ffffff"

gray : String
gray = "#cfcfcf"

red : String
red = "#ff8c93"

yellow : String
yellow = "#f5cb00"

green : String
green = "#c9ff5c"

cyan : String
cyan = "#7dfcff"

blue : String
blue = "#bdc2ff"

purple : String
purple = "#ea4ff9"

darkRed : String
darkRed = "#830026"

darkYellow : String
darkYellow = "#715c00"

darkGreen : String
darkGreen = "#577e00"

darkCyan : String
darkCyan = "#007d80"

darkBlue : String
darkBlue = "#004baf"

darkPurple : String
darkPurple = "#6d0077"

swatchOpacity : Float
swatchOpacity = 0.7

swatchSchemes : Dict (List Int) Scheme
swatchSchemes = Dict.map (always fadeBg) schemes

swatchGray : String
swatchGray = fade swatchOpacity gray

fadeBg : Scheme -> Scheme
fadeBg scheme =
  if scheme.fg == "#000000" then
    { fg = "#000000"
    , f = fade swatchOpacity scheme.f
    , c = fade swatchOpacity scheme.c
    , g = fade swatchOpacity scheme.g
    }
  else
    scheme

fade : Float -> String -> String
fade opacity x =
  String.concat
    [ "#"
    , toHexPair (fadeChannel opacity (parseHexPair 1 x))
    , toHexPair (fadeChannel opacity (parseHexPair 3 x))
    , toHexPair (fadeChannel opacity (parseHexPair 5 x))
    ]

fadeChannel : Float -> Int -> Int
fadeChannel opacity channel =
  round (toFloat channel * opacity + 255 * (1 - opacity))
