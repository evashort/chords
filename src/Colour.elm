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
bg tonic chord =
  case Dict.get chord.flavor schemes of
    Nothing ->
      gray
    Just scheme ->
      case (chord.root - tonic) % 3 of
        0 -> scheme.c
        1 -> scheme.g
        _ -> scheme.f

swatchBg : Int -> Chord -> String
swatchBg tonic chord =
  case Dict.get chord.flavor swatchSchemes of
    Nothing ->
      swatchGray
    Just scheme ->
      case (chord.root - tonic) % 3 of
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
    , ( [ 5, 7 ], sus4 )
    , ( [ 2, 7 ], major7 )
    , ( [ 4, 7, 10 ], dominant7 )
    , ( [ 4, 7, 11 ], major7 )
    , ( [ 3, 7, 10 ], minor7 )
    , ( [ 4, 7, 9 ], minor7 )
    , ( [ 3, 6, 10 ], minor6 )
    , ( [ 3, 7, 9 ], minor6 )
    , ( [ 3, 6, 9 ], diminished )
    , ( [ 3, 7, 11 ], minor7 )
    , ( [ 4, 7, 10, 14 ], dominant9 )
    , ( [ 4, 7, 11, 14 ], major9 )
    , ( [ 3, 7, 10, 14 ], major )
    , ( [ 4, 7, 10, 13 ], dominant9 )
    , ( [ 4, 7, 14 ], major9 )
    , ( [ 3, 7, 14 ], major )
    , ( [ 4, 7, 10, 14, 21 ], dominant7 )
    , ( [ 4, 7, 11, 14, 21 ], major7 )
    , ( [ 3, 7, 10, 14, 21 ], minor7 )
    ]

major : Scheme
major =
  { fg = black
  , f = teal
  , c = purple
  , g = orange
  }

sus4 : Scheme
sus4 =
  { fg = black
  , f = yellow
  , c = cyan
  , g = pink
  }

minor : Scheme
minor =
  { fg = black
  , f = lime
  , c = sky
  , g = rose
  }

major7 : Scheme
major7 =
  { fg = black
  , f = cyan
  , c = pink
  , g = yellow
  }

minor7 : Scheme
minor7 =
  { fg = black
  , f = green
  , c = blue
  , g = red
  }

dominant7 : Scheme
dominant7 =
  { fg = white
  , f = darkTeal
  , c = darkPurple
  , g = darkOrange
  }

minor6 : Scheme
minor6 =
  { fg = white
  , f = darkLime
  , c = darkSky
  , g = darkRose
  }

diminished : Scheme
diminished =
  { fg = white
  , f = darkYellow
  , c = darkCyan
  , g = darkPink
  }

major9 : Scheme
major9 =
  { fg = black
  , f = sky
  , c = rose
  , g = lime
  }

dominant9 : Scheme
dominant9 =
  { fg = white
  , f = darkCyan
  , c = darkPink
  , g = darkYellow
  }

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
red = "#ff997f"
orange : String
orange = "#ffad4c"
yellow : String
yellow = "#e9de28"
lime : String
lime = "#d6f446"
green : String
green = "#bdff8e"
teal : String
teal = "#9effd3"
cyan : String
cyan = "#7dfcff"
sky : String
sky = "#a2e1ff"
blue : String
blue = "#b7caff"
purple : String
purple = "#d0a0ff"
pink : String
pink = "#e86af3"
rose : String
rose = "#ff7da5"

darkRed : String
darkRed = "#822600"
darkOrange : String
darkOrange = "#784c00"
darkYellow : String
darkYellow = "#6b6600"
darkLime : String
darkLime = "#617400"
darkGreen : String
darkGreen = "#4a8525"
darkTeal : String
darkTeal = "#268662"
darkCyan : String
darkCyan = "#007d80"
darkSky : String
darkSky = "#007191"
darkBlue : String
darkBlue = "#0059a7"
darkPurple : String
darkPurple = "#4f139d"
darkPink : String
darkPink = "#6d0077"
darkRose : String
darkRose = "#800038"

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
