module Harp exposing
  ( view, viewBoxLeft, viewBoxRight, isWhiteKey, neckLeft, headLeft
  , borderWidth, headWidth, scale
  )

import Colour

import Html exposing (Html, canvas)
import Html.Attributes as Attributes exposing (style, id, attribute)
import Json.Encode as Encode
import Set exposing (Set)

view : Int -> Int -> Int -> Set Int -> Html msg
view tonic lowestPitch highestPitch pitchSet =
  let
    left = viewBoxLeft lowestPitch - bodyLeftMargin
    right = viewBoxRight highestPitch
  in
  let
    width = right - left
  in
    canvas
      [ id "harp"
      , style "display" "block"
      , style
          "margin-left"
          (String.fromFloat -bodyLeftMargin ++ "px")
      , Attributes.height (round height)
      , attribute
          "spec"
          ( Encode.encode
            0
            ( Encode.object
                [ ( "width", Encode.int (round width) )
                , ( "height", Encode.int (round height) )
                , ( "ropes"
                  , Encode.object
                      ( List.map
                          (encodeRope tonic left)
                          (Set.toList pitchSet)
                      )
                  )
                ]
            )
          )
      ]
      []

encodeRope : Int -> Float -> Int -> (String, Encode.Value)
encodeRope tonic left pitch =
  ( String.fromInt pitch
  , Encode.object
      [ ( "x", Encode.float (neckCenter pitch - left) )
      , ( "color", Encode.string (Colour.ropeColor tonic pitch) )
      ]
  )

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
  (modBy 2 pitch == 1) == (modBy 12 pitch > 4)

neckCenter : Int -> Float
neckCenter pitch =
  let
    pitchClass = modBy 12 pitch
  in
  let
    octave = (pitch - pitchClass) // 12 - 5
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
    pitchClass = modBy 12 pitch
  in
  let
    octave = (pitch - pitchClass) // 12 - 5
    classLeft =
      if pitchClass > 4 then
        toFloat (1 + 4 * pitchClass) * headWidth / 7
      else
        toFloat (modBy 2 pitchClass  + 25 * pitchClass) * headWidth / 42
  in
    classLeft + 7 * headWidth * toFloat octave

headLeft : Int -> Float
headLeft pitch =
  let
    pitchClass = modBy 12 pitch
  in
  let
    octave = (pitch - pitchClass) // 12 - 5
    letterIndex = (pitchClass * 7 + 6) // 12
  in
    headWidth * toFloat (letterIndex + 7 * octave)

borderWidth : Float
borderWidth = 0.5 * scale

headWidth : Float -- includes one border width
headWidth = 7 * scale

height : Float
height = 32 * scale

scale : Float
scale = 6
