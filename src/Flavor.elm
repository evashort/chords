module Flavor exposing (Flavor, get)

import StaffMap exposing (StaffMap)

import Dict exposing (Dict)

get : List Int -> ( Int, Flavor )
get intervals =
  let
    ( before, after ) = splitIntervals intervals
  in
    ( List.length before
    , Maybe.withDefault errorFlavor <|
        Dict.get (after ++ before) flavors
    )

splitIntervals : List Int -> ( List Int, List Int )
splitIntervals intervals =
  case intervals of
    [ 5, 5, 2 ] -> ( [ 5 ], [ 5, 2 ] )
    [ 2, 5, 5 ] -> ( [ 2, 5 ], [ 5 ] )
    [ 4, 2, 4, 2 ] -> ( [], [ 4, 2, 4, 2 ] )
    [ 2, 4, 2, 4 ] -> ( [ 2, 4, 2 ], [ 4 ] )
    _ ->
      case split thirdStart intervals of
        ( _, [] ) -> ( [], intervals )
        result -> result

thirdStart : Int -> Int -> Bool
thirdStart x y =
  ( x == 3 || x == 4, y == 3 || y == 4 ) == ( False, True )

split : (a -> a -> Bool) -> List a -> ( List a, List a )
split pred xs =
  case xs of
    [] -> ( [], [] )
    x :: ys ->
      case ys of
        [] -> ( [ x ], [] )
        y :: _ ->
          if pred x y then ( [ x ], ys )
          else
            let ( before, after ) = split pred ys in
              ( x :: before, after )

black : String
black = "#000000"

white : String
white = "#ffffff"

-- http://www.colourlovers.com/palette/324465/Pastel_Rainbow
-- blue and orange from http://www.colourlovers.com/palette/36070/pastel_rainbow

primary : ( String, String, String )
primary = ( "#facdcd", "#f8facd", "#c9ffff" )

secondary : ( String, String, String )
secondary = ( "#ffe7c9", "#d2facd", "#eccdfa" )

darkPrimary : ( String, String, String )
darkPrimary = ( "#d70000", "#bfc600", "#0047ff" )

darkSecondary : ( String, String, String )
darkSecondary = ( "#e48100", "#15b300", "#9c00e4" )

tertiary : ( String, String, String )
tertiary = ( "#fddacb", "#e5facd", "#dbe6fd" )

tertiary2 : ( String, String, String )
tertiary2 = ( "#fcf1cb", "#cefde6", "#f3cde4" )

darkTertiary : ( String, String, String )
darkTertiary = ( "#de4100", "#6abd00", "#4e24f2" )

lsh3 : Int -> ( a, a, a ) -> ( a, a, a )
lsh3 i ( x, y, z ) =
  case i % 3 of
    0 -> ( x, y, z )
    1 -> ( y, z, x )
    _ -> ( z, x, y )

alwaysFlat : StaffMap
alwaysFlat = [ 0, 1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6 ]

fSharpDFlat : StaffMap
fSharpDFlat = [ 0, 1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6 ]

gSharpEFlat : StaffMap
gSharpEFlat = [ 0, 0, 1, 2, 2, 3, 3, 4, 4, 5, 6, 6 ]

alwaysSharp : StaffMap
alwaysSharp = [ 0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6 ]

type alias Flavor =
  { codeName : String
  , prettyName : String
  , superscript : String
  , staffMap : StaffMap
  , staffOffsets : List Int
  , fg : String
  , bg : ( String, String, String )
  }

errorFlavor : Flavor
errorFlavor =
  { codeName = "err"
  , prettyName = "err"
  , superscript = ""
  , staffMap = alwaysFlat
  , staffOffsets = [ 0, 2, 4 ]
  , fg = white
  , bg = ( "#ff0000", "#ff0000", "#ff0000" )
  }

flavors : Dict (List Int) Flavor
flavors =
  Dict.fromList
    [ ( [ 4, 3, 5 ]
      , { codeName = ""
        , prettyName = ""
        , superscript = ""
        , staffMap = alwaysFlat
        , staffOffsets = [ 0, 2, 4 ]
        , fg = black
        , bg = lsh3 1 primary
        }
      )
    , ( [ 3, 4, 5 ]
      , { codeName = "m"
        , prettyName = "m"
        , superscript = ""
        , staffMap = gSharpEFlat
        , staffOffsets = [ 0, 2, 4 ]
        , fg = black
        , bg = secondary
        }
      )
    , ( [ 3, 3, 6 ]
      , { codeName = "o"
        , prettyName = ""
        , superscript = "o"
        , staffMap = alwaysSharp
        , staffOffsets = [ 0, 2, 4 ]
        , fg = white
        , bg = darkTertiary
        }
      )
    , ( [ 4, 4, 4 ]
      , { codeName = "+"
        , prettyName = "+"
        , superscript = ""
        , staffMap = alwaysFlat
        , staffOffsets = [ 0, 2, 4 ]
        , fg = black
        , bg = lsh3 1 primary
        }
      )
    , ( [ 5, 2, 5 ]
      , { codeName = "sus4"
        , prettyName = ""
        , superscript = "sus4"
        , staffMap = alwaysFlat
        , staffOffsets = [ 0, 3, 4 ]
        , fg = black
        , bg = lsh3 1 primary
        }
      )
    , ( [ 4, 3, 3, 2 ]
      , { codeName = "7"
        , prettyName = ""
        , superscript = "7"
        , staffMap = fSharpDFlat
        , staffOffsets = [ 0, 2, 4, 6 ]
        , fg = white
        , bg = lsh3 1 darkPrimary
        }
      )
    , ( [ 3, 4, 3, 2 ]
      , { codeName = "m7"
        , prettyName = "m"
        , superscript = "7"
        , staffMap = gSharpEFlat
        , staffOffsets = [ 0, 2, 4, 6 ]
        , fg = black
        , bg = tertiary2
        }
      )
    , ( [ 3, 3, 4, 2 ]
      , { codeName = "0"
        , prettyName = ""
        , superscript = "ø"
        , staffMap = alwaysSharp
        , staffOffsets = [ 0, 2, 4, 6 ]
        , fg = white
        , bg = darkSecondary
        }
      )
    , ( [ 4, 3, 4, 1 ]
      , { codeName = "M7"
        , prettyName = ""
        , superscript = "M7"
        , staffMap = alwaysFlat
        , staffOffsets = [ 0, 2, 4, 6 ]
        , fg = black
        , bg = lsh3 1 tertiary
        }
      )
    , ( [ 3, 3, 3, 3 ]
      , { codeName = "o7"
        , prettyName = ""
        , superscript = "o7"
        , staffMap = alwaysSharp
        , staffOffsets = [ 0, 2, 4, 6 ]
        , fg = white
        , bg = darkTertiary
        }
      )
    , ( [ 3, 4, 4, 1 ]
      , { codeName = "mM7"
        , prettyName = "m"
        , superscript = "M7"
        , staffMap = gSharpEFlat
        , staffOffsets = [ 0, 2, 4, 6 ]
        , fg = black
        , bg = secondary
        }
      )
    , ( [ 4, 4, 3, 1 ]
      , { codeName = "+M7"
        , prettyName = "+"
        , superscript = "M7"
        , staffMap = alwaysFlat
        , staffOffsets = [ 0, 2, 4, 6 ]
        , fg = white
        , bg = lsh3 2 darkPrimary
        }
      )
    , ( [ 4, 4, 2, 2 ]
      , { codeName = "+7"
        , prettyName = "+"
        , superscript = "7"
        , staffMap = alwaysFlat
        , staffOffsets = [ 0, 2, 4, 6 ]
        , fg = black
        , bg = lsh3 1 primary
        }
      )
    , ( [ 3, 3, 5, 1 ]
      , { codeName = "mM7b5"
        , prettyName = "m"
        , superscript = "M7♭5"
        , staffMap = alwaysSharp
        , staffOffsets = [ 0, 2, 4, 6 ]
        , fg = white
        , bg = darkTertiary
        }
      )
    , ( [ 4, 2, 4, 2 ]
      , { codeName = "7b5"
        , prettyName = ""
        , superscript = "7♭5"
        , staffMap = fSharpDFlat
        , staffOffsets = [ 0, 2, 4, 6 ]
        , fg = white
        , bg = lsh3 1 darkPrimary
        }
      )
    ]
