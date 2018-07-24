module Keyboard exposing (view)

import Path

import Html exposing (Html)
import Html.Attributes exposing (attribute, style)
import Svg exposing (Svg)
import Svg.Attributes as SA

view : String -> Int -> Int -> Html msg
view gridArea lowestPitch highestPitch =
  let
    left = viewBoxLeft lowestPitch
  in let
    right = viewBoxRight highestPitch
  in let
    width = right - left
  in let
    height = fullHeight + borderWidth
  in
    Svg.svg
      [ style
          [ ( "grid-area", gridArea )
          ]
      , SA.width (toString width)
      , SA.height (toString height)
      , SA.viewBox
          ( String.join
              " "
              [ toString left
              , toString -borderWidth
              , toString width
              , toString height
              ]
          )
      ]
      ( List.concat
          [ [ Svg.rect
                [ SA.x (toString left)
                , SA.y (toString -borderWidth)
                , SA.width (toString width)
                , SA.height (toString height)
                , SA.fill "black"
                ]
                []
            ]
          , List.filterMap
              (viewWhiteKey lowestPitch highestPitch)
              (List.range lowestPitch highestPitch)
          , [ Svg.text_
                [ style
                    [ ( "pointer-events", "none" )
                    ]
                , SA.textAnchor "middle"
                , SA.x
                    (toString (0.5 * (headWidth - borderWidth)))
                , SA.y
                    ( toString
                        ( fullHeight - borderWidth -
                            0.25 * (headWidth - borderWidth)
                        )
                    )
                ]
                [ Svg.text "C4"
                ]
            ]
          ]
      )

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

viewWhiteKey : Int -> Int -> Int -> Maybe (Svg msg)
viewWhiteKey lowestPitch highestPitch pitch =
  if isWhiteKey pitch then
    Just
      ( Svg.path
          [ style
              [ ( "cursor", "pointer" )
              ]
          , attribute "tabindex" "0"
          , SA.fill "white"
          , SA.d (whitePath lowestPitch highestPitch pitch)
          ]
          []
      )
  else
    Nothing

isWhiteKey : Int -> Bool
isWhiteKey pitch =
  (pitch % 2 == 1) == (pitch % 12 > 4)

whitePath : Int -> Int -> Int -> String
whitePath lowestPitch highestPitch pitch =
  String.join
    " "
    [ Path.bigM
        ( if pitch == lowestPitch then
            headLeft pitch
          else
            neckLeft pitch
        )
        0
    , Path.bigV blackHeight
    , Path.bigH (headLeft pitch)
    , Path.bigV (fullHeight - borderWidth - borderRadius)
    , Path.a
        borderRadius borderRadius
        90 False False
        borderRadius borderRadius
    , Path.h (headWidth - borderWidth - 2 * borderRadius)
    , Path.a
        borderRadius borderRadius
        90 False False
        borderRadius -borderRadius
    , Path.bigV blackHeight
    , Path.bigH
        ( if pitch == highestPitch then
            headLeft pitch + headWidth - borderWidth
          else
            neckLeft (pitch + 1) - borderWidth
        )
    , Path.bigV 0
    , Path.bigZ
    ]

-- the origin is the top left corner of middle C, not including its border
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

-- white keys have rounded corners at the bottom
-- the radius is measured at the edge of the white area, inside the border
borderRadius : Float
borderRadius = 0.75 * scale

-- all widths and heights include one border width
headWidth : Float
headWidth = 7 * scale

blackHeight : Float
blackHeight = 20 * scale

fullHeight : Float
fullHeight = 31 * scale

scale : Float
scale = 6
