module Colour exposing (borderOpacity, shineOpacity, fg, bg)

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
  , f = red
  , c = yellow
  , g = blue
  }

sus2 : Scheme
sus2 =
  { fg = black
  , f = yellow
  , c = blue
  , g = red
  }

minor : Scheme
minor =
  { fg = black
  , f = purple
  , c = orange
  , g = green
  }

major7 : Scheme
major7 =
  { fg = black
  , f = mix red orange
  , c = mix yellow green
  , g = mix blue purple
  }

minor7 : Scheme
minor7 =
  { fg = black
  , f = mix purple red
  , c = mix orange yellow
  , g = mix green blue
  }

dominant7 : Scheme
dominant7 =
  { fg = white
  , f = darkRed
  , c = darkYellow
  , g = darkBlue
  }

minor6 : Scheme
minor6 =
  { fg = white
  , f = darkPurple
  , c = darkOrange
  , g = darkGreen
  }

diminished : Scheme
diminished =
  { fg = white
  , f = mix darkBlue darkPurple
  , c = mix darkRed darkOrange
  , g = mix darkYellow darkGreen
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
gray = "#e0e0e0"

red : String
red = "#facdcd"

orange : String
orange = "#ffe7c9"

yellow : String
yellow = "#f8facd"

green : String
green = "#d2facd"

blue : String
blue = "#c9ffff"

purple : String
purple = "#eccdfa"

darkRed : String
darkRed = "#d70000"

darkOrange : String
darkOrange = "#e48100"

darkYellow : String
darkYellow = "#bfc600"

darkGreen : String
darkGreen = "#15b300"

darkBlue : String
darkBlue = "#0047ff"

darkPurple : String
darkPurple = "#9c00e4"
