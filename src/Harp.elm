module Harp exposing
  ( view, viewBoxLeft, viewBoxRight, isWhiteKey, neckLeft, headLeft
  , borderWidth, headWidth, scale
  )

import Colour

import Html exposing (Html)
import Html.Attributes exposing (style, id)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA

view : Int -> Int -> Int -> Set Int -> Html msg
view tonic lowestPitch highestPitch pitchSet =
  let
    left = viewBoxLeft lowestPitch - bodyLeftMargin
  in let
    right = viewBoxRight highestPitch
  in let
    width = right - left
  in
    Svg.svg
      [ SA.width (toString width)
      , SA.height (toString height)
      , SA.viewBox
          ( String.join
              " "
              [ toString left
              , "0"
              , toString width
              , toString height
              ]
          )
      , style
          [ ( "display", "block" )
          , ( "margin-left", toString -bodyLeftMargin ++ "px")
          ]
      , id "harp"
      ]
      ( List.concat
          [ [ Svg.rect
                [ SA.x (toString left)
                , SA.y "0"
                , SA.width (toString width)
                , SA.height (toString height)
                , SA.fill "#eeeeee"
                ]
                []
            ]
          , List.map (viewRope tonic) (Set.toList pitchSet)
          , [ Svg.text_
                [ style
                    [ ( "pointer-events", "none" )
                    ]
                , SA.fill "GrayText"
                , SA.textAnchor "middle"
                , SA.x
                    ( toString
                        (left + bodyLeftMargin + 10 * headWidth)
                    )
                , SA.y (toString (0.5 * height + 6))
                ]
                [ Svg.text "Drag across strings to strum"
                ]
            ]
          ]
      )

viewRope : Int -> Int -> Svg msg
viewRope tonic pitch =
  let
    x = neckCenter pitch
  in
    Svg.path
      [ SA.class "rope"
      , SA.name (toString pitch)
      , SA.transform ("translate(" ++ toString x ++ " 0)")
      , SA.d ("M0,0 V" ++ toString height)
      , SA.stroke (Colour.ropeColor tonic pitch)
      , SA.strokeWidth "4"
      ]
      []

bodyLeftMargin : Float
bodyLeftMargin = 8

viewBoxLeft : Int -> Float
viewBoxLeft lowestPitch =
  if isWhiteKey lowestPitch then
    headLeft lowestPitch - borderWidth
  else
    neckLeft lowestPitch - borderWidth

viewBoxRight : Int -> Float
viewBoxRight highestPitch =
  if isWhiteKey highestPitch then
    headLeft highestPitch + headWidth
  else
    neckLeft (highestPitch + 1)

isWhiteKey : Int -> Bool
isWhiteKey pitch =
  (pitch % 2 == 1) == (pitch % 12 > 4)

neckCenter : Int -> Float
neckCenter pitch =
  let
    pitchClass = pitch % 12
  in let
    octave = (pitch - pitchClass) // 12 - 5
  in let
    classLeft =
      if pitchClass > 4 then
        toFloat (3 + 4 * pitchClass) * headWidth / 7
      else
        toFloat (27 + 50 * pitchClass) * headWidth / 84
  in
    classLeft + 7 * headWidth * toFloat octave - 0.5 * borderWidth

neckLeft : Int -> Float
neckLeft pitch =
  let
    pitchClass = pitch % 12
  in let
    octave = (pitch - pitchClass) // 12 - 5
  in let
    classLeft =
      if pitchClass > 4 then
        toFloat (1 + 4 * pitchClass) * headWidth / 7
      else
        toFloat (pitchClass % 2 + 25 * pitchClass) * headWidth / 42
  in
    classLeft + 7 * headWidth * toFloat octave

headLeft : Int -> Float
headLeft pitch =
  let
    pitchClass = pitch % 12
  in let
    octave = (pitch - pitchClass) // 12 - 5
  in let
    letterIndex = (pitchClass * 7 + 6) // 12
  in
    headWidth * toFloat (letterIndex + 7 * octave)

borderWidth : Float
borderWidth = 0.5 * scale

headWidth : Float -- includes one border width
headWidth = 7 * scale

height : Float
height = 45 * scale

scale : Float
scale = 6
