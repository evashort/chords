module Circle exposing (view)

import Chord exposing (Chord)
import ChordView
import Colour
import CustomEvents exposing (isAudioTimeButton, onClickWithAudioTime)
import IdChord exposing (IdChord)
import Path
import Player exposing (Player)
import Name

import Html exposing (Html)
import Html.Attributes as Attributes exposing (attribute, style)
import Svg exposing (Svg, svg, defs, linearGradient, path, text_, rect)
import Svg.Attributes exposing
  ( width, height, viewBox
  , d, fill, opacity
  , stroke, strokeWidth, strokeLinejoin, strokeDasharray, strokeOpacity
  , x1, y1, x2, y2
  , textAnchor
  )

view : Int -> Player -> Maybe IdChord -> Html ChordView.Msg
view tonic player selection =
  let
    rInner = 100
    rOuter = 247.5
    rMid = areaAverage 100 247.5
    majorChords =
      List.filterMap
        ( IdChord.fromChord <<
            Chord [ 4, 7 ] <<
            (\i -> modBy 12 (tonic + 7 * i))
        )
        (List.range 0 11)
    minorChords =
      List.filterMap
        ( IdChord.fromChord <<
            Chord [ 3, 7 ] <<
            (\i -> modBy 12 (9 + tonic + 7 * i))
        )
        (List.range 0 11)
  in
    Html.span
      [ Attributes.id "circlePane"
      , style "position" "relative"
      , style "display" "block"
      ]
      [ Svg.svg
          [ width "500"
          , height "500"
          , viewBox "0 0 500 500"
          , style "display" "block"
          ]
          ( List.concat
              [ [ gradients ]
              , scaleShadow
              , List.concat
                  ( List.indexedMap
                      (viewChord tonic player selection rMid rOuter)
                      majorChords
                  )
              , List.concat
                  ( List.indexedMap
                      (viewChord tonic player selection rInner rMid)
                      minorChords
                  )
              ]
          )
      , Html.span
          [ style "display" "block"
          , style "pointer-events" "none"
          , style "text-align" "center"
          , style "font-size" "150%"
          ]
          ( List.concat
              [ List.indexedMap
                  (viewChordText player (0.5 * (rMid + rOuter)))
                  majorChords
              , List.indexedMap
                  (viewChordText player (0.5 * (rInner + rMid)))
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
            , style "stop-color" "white"
            , style "stop-opacity" "1"
            ]
            []
        , Svg.stop
            [ Svg.Attributes.offset "60%"
            , style "stop-color" "white"
            , style "stop-opacity" "0"
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
  Int -> Player -> Maybe IdChord -> Float -> Float -> Int -> IdChord ->
    List (Svg ChordView.Msg)
viewChord tonic player selection rInner rOuter i idChord =
  let
    solid =
      Player.active player == Just idChord || selection == Just idChord
    dashed = Player.next player == Just idChord.id
    stoppable =
      Player.stoppable player &&
        Player.active player == Just idChord
  in
    List.filterMap
      identity
      [ if solid || dashed then
          Just
            ( path
                [ fill "none"
                , stroke "#3399ff"
                , strokeWidth "5"
                , strokeLinejoin "round"
                , strokeDasharray
                    ( if dashed then
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
          action =
            if stoppable then
              ChordView.Stop
            else
              ChordView.Play << Tuple.pair idChord
        in
          Just
            ( path
                [ isAudioTimeButton True
                , onClickWithAudioTime action
                , fill (Colour.bg tonic idChord.chord)
                , attribute "tabindex" "0"
                , style "cursor" "pointer"
                , d (twelfth 5 rInner rOuter i)
                ]
                []
            )
      , Just
          ( path
              [ fill "url(#twelfthShine)"
              , opacity (Colour.shineOpacity idChord.chord)
              , d (twelfth 7 rInner rOuter i)
              , style "pointer-events" "none"
              ]
              []
          )
      , Just
          ( path
              [ fill "none"
              , stroke "black"
              , strokeOpacity (Colour.borderOpacity idChord.chord)
              , d (twelfth 6 rInner rOuter i)
              , style "pointer-events" "none"
              ]
              []
          )
      ]


viewChordText : Player -> Float -> Int -> IdChord -> Html msg
viewChordText player r i idChord =
  let
    ( x, y ) =
      let a = 2 * pi * (0.25 - toFloat i / 12) in
        ( polarX r a, polarY r a )
    stoppable =
      Player.stoppable player &&
        Player.active player == Just idChord
  in
    Html.span
      [ style "position"  "absolute"
      , style "left" (String.fromFloat (x - 0.5 * 75) ++ "px")
      , style "top" (String.fromFloat (y - 0.5 * 75) ++ "px")
      , style "width" "75px"
      , style "line-height" "75px"
      , style "color" (Colour.fg idChord.chord)
      ]
      ( if stoppable then
          [ Html.span
              [ style "width" "1em"
              , style "height" "1em"
              , style "display" "inline-block"
              , style "vertical-align" "middle"
              , style "background" (Colour.fg idChord.chord)
              ]
              []
          ]
        else
          Name.view idChord.chord
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
    [ Path.bigM
        (polarX rOuter earlyOuter)
        (polarY rOuter earlyOuter)
    , Path.bigA
        rOuter rOuter
        0
        False True
        (polarX rOuter lateOuter)
        (polarY rOuter lateOuter)
    , Path.bigL
        (polarX rInner lateInner)
        (polarY rInner lateInner)
    , Path.bigA
        rInner rInner
        0
        False False
        (polarX rInner earlyInner)
        (polarY rInner earlyInner)
    , Path.bigZ
    ]

polarX : Float -> Float -> Float
polarX r a =
  250 + r * cos a

polarY : Float -> Float -> Float
polarY r a =
   250 - r * sin a
