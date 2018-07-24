module Path exposing (bigM, h, bigH, bigV, a, bigZ)

bigM : Float -> Float -> String
bigM x y =
  "M" ++ toString x ++ "," ++ toString y

h : Float -> String
h dx =
  "h" ++ toString dx

bigH : Float -> String
bigH x =
  "H" ++ toString x

bigV : Float -> String
bigV y =
  "V" ++ toString y

a : Float -> Float -> Float -> Bool -> Bool -> Float -> Float -> String
a rx ry angle largeArc sweep dx dy =
  String.concat
    [ "a"
    , toString rx
    , ","
    , toString ry
    , " "
    , toString angle
    , " "
    , if largeArc then "1" else "0"
    , " "
    , if sweep then "1" else "0"
    , " "
    , toString dx
    , ","
    , toString dy
    ]

bigZ : String
bigZ = "Z"
