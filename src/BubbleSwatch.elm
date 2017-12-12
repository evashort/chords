module BubbleSwatch exposing (BubbleSwatch(..), view)

import Swatch exposing (Swatch)

import Html exposing (Html, span, text)
import Html.Attributes exposing (style)
import Svg exposing (svg, polyline)
import Svg.Attributes exposing
  (width, height, viewBox, fill, stroke, points)

type BubbleSwatch
  = Bubble String
  | JustSwatch Swatch

view : BubbleSwatch -> Html msg
view bubbleSwatch =
  case bubbleSwatch of
    Bubble bubble ->
      viewBubble bubble
    JustSwatch swatch ->
      Swatch.view swatch

viewBubble : String -> Html msg
viewBubble bubble =
  span
    [ style
        [ ( "position", "relative" )
        , ( "pointer-events", "none" )
        , ( "color", "black")
        , ( "font-family", "Arial, Helvetica, sans-serif" )
        , ( "font-size", "10pt" )
        ]
    ]
    [ span
        [ style
            [ ( "position", "absolute" )
            , ( "z-index", "2" )
            , ( "top", "calc(100% + 8px)" )
            , ( "background", "white" )
            , ( "border", "1px solid darkgray" )
            , ( "border-radius", "0px 3px 3px 3px" )
            , ( "box-shadow", "1px 1px 4px rgba(0, 0, 0, 0.4)")
            , ( "padding", "3px" )
            , ( "white-space", "nowrap" )
            ]
        ]
        [ text bubble ]
    , svg
        [ style
            [ ( "position", "absolute" )
            , ( "z-index", "2" )
            , ( "top", "100%" )
            ]
        , width "9"
        , height "9"
        , viewBox "0 0 9 9"
        ]
        [ polyline
            [ fill "white"
            , stroke "darkgray"
            , points "0.5,9 0.5,0.5 9,9"
            ]
            []
        ]
    ]
