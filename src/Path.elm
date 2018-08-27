module Path exposing
  (m, bigM, l, bigL, h, bigH, v, bigV, partialC, c, a, bigA, bigZ)

m : Float -> Float -> String
m dx dy =
  "m" ++ String.fromFloat dx ++ "," ++ String.fromFloat dy

bigM : Float -> Float -> String
bigM x y =
  "M" ++ String.fromFloat x ++ "," ++ String.fromFloat y

l : Float -> Float -> String
l dx dy =
  "l" ++ String.fromFloat dx ++ "," ++ String.fromFloat dy

bigL : Float -> Float -> String
bigL x y =
  "L" ++ String.fromFloat x ++ "," ++ String.fromFloat y

h : Float -> String
h dx =
  "h" ++ String.fromFloat dx

bigH : Float -> String
bigH x =
  "H" ++ String.fromFloat x

v : Float -> String
v dy =
  "v" ++ String.fromFloat dy

bigV : Float -> String
bigV y =
  "V" ++ String.fromFloat y

partialC :
  Float -> Float -> Float -> Float -> Float -> Float -> Float -> String
partialC t xB yB xC yC xD yD =
  let
    xE = t * xB
    yE = t * yB
    xF = (1 - t) * xB + t * xC
    yF = (1 - t) * yB + t * yC
    xG = (1 - t) * xC + t * xD
    yG = (1 - t) * yC + t * yD
  in
  let
    xH = (1 - t) * xE + t * xF
    yH = (1 - t) * yE + t * yF
    xI = (1 - t) * xF + t * xG
    yI = (1 - t) * yF + t * yG
  in
  let
    xJ = (1 - t) * xH + t * xI
    yJ = (1 - t) * yH + t * yI
  in
    c xE yE xH yH xJ yJ

c : Float -> Float -> Float -> Float -> Float -> Float -> String
c dx1 dy1 dx2 dy2 dx dy =
  String.concat
    [ "c"
    , String.fromFloat dx1
    , ","
    , String.fromFloat dy1
    , " "
    , String.fromFloat dx2
    , ","
    , String.fromFloat dy2
    , " "
    , String.fromFloat dx
    , ","
    , String.fromFloat dy
    ]

a : Float -> Float -> Float -> Bool -> Bool -> Float -> Float -> String
a rx ry angle largeArc sweep dx dy =
  String.concat
    [ "a"
    , String.fromFloat rx
    , ","
    , String.fromFloat ry
    , " "
    , String.fromFloat angle
    , " "
    , if largeArc then "1" else "0"
    , " "
    , if sweep then "1" else "0"
    , " "
    , String.fromFloat dx
    , ","
    , String.fromFloat dy
    ]

bigA : Float -> Float -> Float -> Bool -> Bool -> Float -> Float -> String
bigA rx ry angle largeArc sweep x y =
  String.concat
    [ "A"
    , String.fromFloat rx
    , ","
    , String.fromFloat ry
    , " "
    , String.fromFloat angle
    , " "
    , if largeArc then "1" else "0"
    , " "
    , if sweep then "1" else "0"
    , " "
    , String.fromFloat x
    , ","
    , String.fromFloat y
    ]

bigZ : String
bigZ = "Z"
