module CircleOfFifths exposing (chordCount, view, Msg(..))

import CachedChord
import Chord exposing (Chord)
import ChordParser exposing (IdChord)
import CustomEvents exposing (onLeftDown, onLeftClick, onKeyDown)
import Player exposing (PlayStatus)

import Html exposing (Html)
import Html.Attributes exposing (attribute, style)
import Html.Events exposing (on)
import Svg exposing (Svg, svg, defs, linearGradient, path, text_, rect)
import Svg.Attributes exposing
  ( width, height, viewBox
  , d, fill, opacity
  , stroke, strokeWidth, strokeLinejoin, strokeDasharray, strokeOpacity
  , x1, y1, x2, y2
  , textAnchor
  )

chordCount : Int
chordCount = 24

majorChords : List IdChord
majorChords = List.map (nthMajorChord 0) (List.range 0 11)

nthMajorChord : Int -> Int -> IdChord
nthMajorChord firstId i =
  { id = firstId + i
  , cache =
      CachedChord.fromChord
        ( List.map
            ((+) ((7 * i) % 12))
            [ 48, 52, 55 ]
        )
  }

minorChords : List IdChord
minorChords = List.map (nthMinorChord 12) (List.range 0 11)

nthMinorChord : Int -> Int -> IdChord
nthMinorChord firstId i =
  { id = firstId + i
  , cache =
      CachedChord.fromChord
        ( List.map
            ((+) ((7 * i + 9) % 12))
            [ 48, 51, 55 ]
        )
  }

type Msg
  = PlayChord ( Chord, Int )
  | StopChord

view : PlayStatus -> Html Msg
view playStatus =
  Svg.svg
      [ width "500"
      , height "500"
      , viewBox "0 0 500 500"
      , style [ ( "font-size", "18pt" ) ]
      ]
      ( List.concat
          [ [ defs []
                [ linearGradient
                    [ Svg.Attributes.id "twelfthShine"
                    , x1 "0%"
                    , y1 "0%"
                    , x2 "10%"
                    , y2 "100%"
                    ]
                    [ Svg.stop
                        [ Svg.Attributes.offset "0%"
                        , style
                            [ ( "stop-color", "white" )
                            , ( "stop-opacity", "1" )
                            ]
                        ]
                        []
                    , Svg.stop
                        [ Svg.Attributes.offset "60%"
                        , style
                            [ ( "stop-color", "white" )
                            , ( "stop-opacity", "0" )
                            ]
                        ]
                        []
                    ]
                ]
            , path
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
          , List.concatMap
              List.concat
              [ List.indexedMap
                  (viewChord playStatus (areaAverage 100 247.5) 247.5)
                  majorChords
              , List.indexedMap
                  (viewChord playStatus 100 (areaAverage 100 247.5))
                  minorChords
              ]
          ]
      )

areaAverage : Float -> Float -> Float
areaAverage x y =
  sqrt (0.5 * (x * x + y * y))

viewChord : PlayStatus -> Float -> Float -> Int -> IdChord -> List (Svg Msg)
viewChord playStatus rInner rOuter i chord =
  List.filterMap
    identity
    [ if playStatus.active == chord.id || playStatus.next == chord.id then
        Just
          ( path
              [ fill "none"
              , stroke "#3399ff"
              , strokeWidth "5"
              , strokeLinejoin "round"
              , strokeDasharray
                  (if playStatus.next == chord.id then "10, 10" else "none")
              , d (twelfth 0 rInner rOuter i)
              ]
              []
          )
      else
        Nothing
    , let
        stopButton = playStatus.active == chord.id && playStatus.stoppable
      in let
        play =
          if stopButton then StopChord
          else PlayChord ( chord.cache.chord, chord.id )
      in
        Just
          ( path
              [ onLeftDown play
              , onKeyDown
                  [ ( 13, play )
                  , ( 32, play )
                  ]
              , fill (CachedChord.bg chord.cache)
              , attribute "tabindex" "0"
              , style [ ( "cursor", "pointer" ) ]
              , d (twelfth 5 rInner rOuter i)
              ]
              []
          )
    , Just
        ( path
            [ fill "url(#twelfthShine)"
            , opacity (CachedChord.shineOpacity chord.cache)
            , d (twelfth 7 rInner rOuter i)
            , style [ ( "pointer-events", "none" ) ]
            ]
            []
        )
    , Just
        ( path
            [ fill "none"
            , stroke "black"
            , strokeOpacity (CachedChord.borderOpacity chord.cache)
            , d (twelfth 6 rInner rOuter i)
            , style [ ( "pointer-events", "none" ) ]
            ]
            []
        )
    , let
        stopButton = playStatus.active == chord.id && playStatus.stoppable
      in let
        ( x, y ) =
          polar
            (0.5 * (rInner + rOuter))
            (2 * pi * (0.25 - toFloat i / 12))
      in
        Just
          ( if stopButton then
              rect
                [ Svg.Attributes.x (toString (x - 10))
                , Svg.Attributes.y (toString (y - 10))
                , width "20"
                , height "20"
                , style [ ( "pointer-events", "none" ) ]
                ]
                []
            else
              text_
                [ Svg.Attributes.x (toString x)
                , Svg.Attributes.y (toString (y + 9))
                , textAnchor "middle"
                , style [ ( "pointer-events", "none" ) ]
                ]
                [ Svg.text
                    ( chord.cache.prettyNamesake ++
                        chord.cache.flavor.prettyName
                    )
                ]
          )
    ]

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
