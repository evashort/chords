module Path exposing (m, bigM, h, bigH, v, bigV, c, a, bigZ)

m : Float -> Float -> String
m dx dy =
  "m" ++ toString dx ++ "," ++ toString dy

bigM : Float -> Float -> String
bigM x y =
  "M" ++ toString x ++ "," ++ toString y

h : Float -> String
h dx =
  "h" ++ toString dx

bigH : Float -> String
bigH x =
  "H" ++ toString x

v : Float -> String
v dy =
  "v" ++ toString dy

bigV : Float -> String
bigV y =
  "V" ++ toString y

c : Float -> Float -> Float -> Float -> Float -> Float -> String
c dx1 dy1 dx2 dy2 dx dy =
  String.concat
    [ "c"
    , toString dx1
    , ","
    , toString dy1
    , " "
    , toString dx2
    , ","
    , toString dy2
    , " "
    , toString dx
    , ","
    , toString dy
    ]

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
