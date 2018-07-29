module Circle exposing (view)

import Chord exposing (Chord)
import Colour
import CustomEvents exposing (onLeftDown, onKeyDown)
import IdChord exposing (IdChord)
import PlayStatus exposing (PlayStatus)
import Name

import Html exposing (Html)
import Html.Attributes exposing (attribute, style)
import Svg exposing (Svg, svg, defs, linearGradient, path, text_, rect)
import Svg.Attributes exposing
  ( width, height, viewBox
  , d, fill, opacity
  , stroke, strokeWidth, strokeLinejoin, strokeDasharray, strokeOpacity
  , x1, y1, x2, y2
  , textAnchor
  )

view : String -> Int -> PlayStatus -> Html IdChord.Msg
view gridArea tonic playStatus =
  let
    rInner = 100
  in let
    rOuter = 247.5
  in let
    rMid = areaAverage 100 247.5
  in let
    majorChords =
      List.map
        ( IdChord.fromChord <<
            Chord [ 4, 7 ] <<
            (\i -> (tonic + 7 * i) % 12)
        )
        (List.range 0 11)
  in let
    minorChords =
      List.map
        ( IdChord.fromChord <<
            Chord [ 3, 7 ] <<
            (\i -> (9 + tonic + 7 * i) % 12)
        )
        (List.range 0 11)
  in
    Html.span
      [ style
          [ ( "grid-area", gridArea )
          , ( "position", "relative" )
          , ( "font-size", "150%" )
          ]
      ]
      [ Svg.svg
          [ width "500"
          , height "500"
          , viewBox "0 0 500 500"
          ]
          ( List.concat
              [ [ gradients ]
              , scaleShadow
              , List.concat
                  ( List.indexedMap
                      (viewChord tonic playStatus rMid rOuter)
                      majorChords
                  )
              , List.concat
                  ( List.indexedMap
                      (viewChord tonic playStatus rInner rMid)
                      minorChords
                  )
              ]
          )
      , Html.span
          [ style
              [ ( "position", "absolute" )
              , ( "top", "0" )
              , ( "left", "0" )
              , ( "bottom", "0" )
              , ( "right", "0" )
              , ( "pointer-events", "none" )
              , ( "text-align", "center" )
              ]
          ]
          ( List.concat
              [ List.indexedMap
                  (viewChordText playStatus (0.5 * (rMid + rOuter)))
                  majorChords
              , List.indexedMap
                  (viewChordText playStatus (0.5 * (rInner + rMid)))
                  minorChords
              ]
          )
      ]

gradients : Svg msg
gradients =
  defs []
    [ linearGradient
        [ Svg.Attributes.id "twelfthShine"
        , x1 "0%", y1 "0%", x2 "10%", y2 "100%"
        ]
        [ Svg.stop
            [ Svg.Attributes.offset "0%"
            , style [ ( "stop-color", "white" ), ( "stop-opacity", "1" ) ]
            ]
            []
        , Svg.stop
            [ Svg.Attributes.offset "60%"
            , style [ ( "stop-color", "white" ), ( "stop-opacity", "0" ) ]
            ]
            []
        ]
    ]

scaleShadow : List (Svg msg)
scaleShadow =
  [ path
      [ fill "lightgray"
      , stroke "lightgray"
      , strokeWidth "5"
      , strokeLinejoin "round"
      , d
          ( paddedWedge 5
              97.5 250
              (2 * pi * 9 / 24) (2 * pi * 3 / 24)
          )
      ]
      []
  , path
      [ fill "lightgray"
      , stroke "lightgray"
      , strokeWidth "5"
      , strokeLinejoin "round"
      , d
          ( paddedWedge 5
              (areaAverage 100 247.5) 250
              (2 * pi * -1 / 24) (2 * pi * -3 / 24)
          )
      ]
      []
  ]

areaAverage : Float -> Float -> Float
areaAverage x y =
  sqrt (0.5 * (x * x + y * y))

viewChord :
  Int -> PlayStatus -> Float -> Float -> Int -> IdChord ->
    List (Svg IdChord.Msg)
viewChord tonic playStatus rInner rOuter i { id, chord } =
  List.filterMap
    identity
    [ if PlayStatus.hasBorder playStatus id then
        Just
          ( path
              [ fill "none"
              , stroke "#3399ff"
              , strokeWidth "5"
              , strokeLinejoin "round"
              , strokeDasharray
                  ( if PlayStatus.hasDashedBorder playStatus id then
                      "10, 10"
                    else
                      "none"
                  )
              , d (twelfth 0 rInner rOuter i)
              ]
              []
          )
      else
        Nothing
    , let
        play =
          if PlayStatus.hasStopButton playStatus id then
            IdChord.Stop
          else
            IdChord.Play (IdChord id chord)
      in
        Just
          ( path
              [ onLeftDown play
              , onKeyDown
                  [ ( 13, play )
                  , ( 32, play )
                  ]
              , fill (Colour.bg tonic chord)
              , attribute "tabindex" "0"
              , style [ ( "cursor", "pointer" ) ]
              , d (twelfth 5 rInner rOuter i)
              ]
              []
          )
    , Just
        ( path
            [ fill "url(#twelfthShine)"
            , opacity (Colour.shineOpacity chord)
            , d (twelfth 7 rInner rOuter i)
            , style [ ( "pointer-events", "none" ) ]
            ]
            []
        )
    , Just
        ( path
            [ fill "none"
            , stroke "black"
            , strokeOpacity (Colour.borderOpacity chord)
            , d (twelfth 6 rInner rOuter i)
            , style [ ( "pointer-events", "none" ) ]
            ]
            []
        )
    ]


viewChordText : PlayStatus -> Float -> Int -> IdChord -> Html msg
viewChordText playStatus r i { id, chord } =
  let
    ( x, y ) =
      polar r (2 * pi * (0.25 - toFloat i / 12))
  in
    Html.span
      [ style
          [ ( "position", "absolute" )
          , ( "left", toString (x - 0.5 * 75) ++ "px" )
          , ( "top", toString (y - 0.5 * 75) ++ "px" )
          , ( "width", "75px" )
          , ( "line-height", "75px" )
          , ( "color", Colour.fg chord )
          ]
      ]
      ( if PlayStatus.hasStopButton playStatus id then
          [ Html.span
              [ style
                  [ ( "width", "1em" )
                  , ( "height", "1em" )
                  , ( "display", "inline-block" )
                  , ( "vertical-align", "middle" )
                  , ( "background", Colour.fg chord )
                  ]
              ]
              []
          ]
        else
          Name.view chord
      )

twelfth : Float -> Float -> Float -> Int -> String
twelfth padding rInner rOuter i =
  paddedWedge
    padding
    rInner
    rOuter
    (2 * pi * (0.25 - (-0.5 + toFloat i) / 12))
    (2 * pi * (0.25 - (0.5 + toFloat i) / 12))

paddedWedge : Float -> Float -> Float -> Float -> Float -> String
paddedWedge padding rInner rOuter early late =
  let
    innerPadding = 0.5 * padding / rInner
  in let
    outerPadding = 0.5 * padding / rOuter
  in
    wedge
      (rInner + 0.5 * padding) (rOuter - 0.5 * padding)
      (early - innerPadding) (early - outerPadding)
      (late + innerPadding) (late + outerPadding)

wedge : Float -> Float -> Float -> Float -> Float -> Float -> String
wedge rInner rOuter earlyInner earlyOuter lateInner lateOuter =
  String.join
    " "
    [ moveTo (polar rOuter earlyOuter)
    , arc rOuter True (polar rOuter lateOuter)
    , lineTo (polar rInner lateInner)
    , arc rInner False (polar rInner earlyInner)
    , closePath
    ]

polar : Float -> Float -> ( Float, Float )
polar r a =
  ( 250 + r * cos a, 250 - r * sin a )

moveTo : ( Float, Float ) -> String
moveTo ( x, y ) =
  "M" ++ toString x ++ "," ++ toString y

lineTo : ( Float, Float ) -> String
lineTo ( x, y ) =
  "L" ++ toString x ++ "," ++ toString y

arc : Float -> Bool -> ( Float, Float ) -> String
arc r clockwise ( x, y ) =
  String.concat
    [ "A"
    , toString r
    , ","
    , toString r
    , " 0 0,"
    , if clockwise then "1" else "0"
    , " "
    , toString x
    , ","
    , toString y
    ]

closePath : String
closePath = "Z"
