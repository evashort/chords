module Highlight exposing (Highlight, view, mergeLayers)

import Substring exposing (Substring)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, polyline)
import Svg.Attributes exposing
  (width, height, viewBox, fill, stroke, points)

type alias Highlight =
  { bubbleText : String
  , fg : String
  , bg : String
  , substring : Substring
  }

view : Highlight -> Html msg
view highlight =
  if highlight.bubbleText /= "" && highlight.substring.s == "" then
    viewBubble highlight.bubbleText
  else
    viewNoBubble highlight

viewBubble : String -> Html msg
viewBubble bubbleText =
  Html.span
    [ style
        [ ( "position", "relative" )
        , ( "pointer-events", "none" )
        , ( "color", "black")
        , ( "font-family", "Arial, Helvetica, sans-serif" )
        , ( "font-size", "10pt" )
        ]
    ]
    [ Html.span
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
        [ Html.text bubbleText ]
    , Svg.svg
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

viewNoBubble : Highlight -> Html msg
viewNoBubble highlight =
  case ( highlight.fg, highlight.bg ) of
    ( "#000000", "#ffffff" ) ->
      Html.text highlight.substring.s
    ( "#000000", _ ) ->
      Html.mark
        [ style
            [ ( "color", "inherit" )
            , ( "background", highlight.bg )
            , ( "border-radius", "3px" )
            ]
        ]
        [ Html.text highlight.substring.s ]
    ( _, "#ffffff" ) ->
      Html.mark
        [ style
            [ ( "color", highlight.fg )
            , ( "background", "transparent" )
            , ( "text-shadow", "0px 0px 0px white" )
            , ( "position", "relative" )
            , ( "pointer-events", "none" )
            ]
        ]
        [ Html.text highlight.substring.s ]
    _ ->
      Html.mark
        [ style
            [ ( "color", "inherit" )
            , ( "background", highlight.bg )
            , ( "border-radius", "3px" )
            ]
        ]
        [ Html.mark
            [ style
                [ ( "color", highlight.fg )
                , ( "background", "transparent" )
                , ( "text-shadow", "0px 0px 0px " ++ highlight.bg )
                , ( "position", "relative" )
                , ( "pointer-events", "none" )
                ]
            ]
            [ Html.text highlight.substring.s ]
        ]

mergeLayers : List (List Highlight) -> List Highlight
mergeLayers layers =
  case
    pop
      { above = []
      , belowStart = 0
      , below = List.map (List.sortBy (.i << .substring)) layers
      }
  of
    Nothing ->
      []
    Just ( highlight, queue ) ->
      mergeLayersHelp highlight queue

mergeLayersHelp : Highlight -> Queue -> List Highlight
mergeLayersHelp highlight queue =
  case pop queue of
    Nothing ->
      [ highlight ]
    Just ( highlightNew, queueNew ) ->
      { highlight
      | substring =
          Substring.before
            highlightNew.substring.i
            highlight.substring
      } ::
        mergeLayersHelp highlightNew queueNew

type alias Queue =
  { above : List ( Highlight, List Highlight )
  , belowStart : Int
  , below : List (List Highlight)
  }

pop : Queue -> Maybe ( Highlight, Queue )
pop queue =
  case popAbove queue of
    Just result ->
      Just result
    Nothing ->
      popBelow queue

popAbove : Queue -> Maybe ( Highlight, Queue )
popAbove queue =
  let
    ( belowNew, layerAndAboveNew ) =
      splitBeforeLastMin (.i << .substring << Tuple.first) queue.above
  in
    case layerAndAboveNew of
      ( highlight, peers ) :: aboveNew ->
        if highlight.substring.i <= queue.belowStart then
          Just
            ( highlight
            , { queue
              | above = aboveNew
              , belowStart = Substring.stop highlight.substring
              , below =
                  ( if highlight.substring.s == "" then
                      peers
                    else
                      highlight :: peers
                  ) ::
                    List.reverse (List.map cons belowNew) ++
                      queue.below
              }
            )
        else
          Nothing
      [] ->
        Nothing

popBelow : Queue -> Maybe ( Highlight, Queue )
popBelow queue =
  case queue.below of
    [] ->
      Nothing
    layer :: belowNew ->
      case layerAfter queue.belowStart layer of
        Nothing ->
          popBelow { queue | below = belowNew }
        Just ( highlight, peers ) ->
          if highlight.substring.i == queue.belowStart then
            Just
              ( highlight
              , { queue
                | belowStart = Substring.stop highlight.substring
                , below =
                    if highlight.substring.s == "" then
                      peers :: belowNew
                    else
                      queue.below
                }
              )
          else
            popBelow
              { queue
              | above = ( highlight, peers ) :: queue.above
              , below = belowNew
              }

layerAfter :
  Int -> List Highlight -> Maybe ( Highlight, List Highlight )
layerAfter start layer =
  case layer of
    [] ->
      Nothing
    highlight :: peers ->
      if highlight.substring.i >= start then -- separate case to avoid
        Just ( highlight, peers ) -- removing zero-length highlights
      else
        let
          substringNew = Substring.after start highlight.substring
        in
          if substringNew.s == "" then
            layerAfter start peers
          else
            Just ( { highlight | substring = substringNew }, peers )

splitBeforeLastMin : (a -> comparable) -> List a -> ( List a, List a )
splitBeforeLastMin f xs =
  case xs of
    [] ->
      ( [], [] )
    x :: rest ->
      case splitBeforeLastMinHelp (f x) f rest of
        Nothing ->
          ( [], xs )
        Just ( before, after ) ->
          ( x :: before, after )

splitBeforeLastMinHelp :
  comparable -> (a -> comparable) -> List a -> Maybe ( List a, List a )
splitBeforeLastMinHelp oldMin f xs =
  case xs of
    [] ->
      Nothing
    x :: rest ->
      let v = f x in
        case splitBeforeLastMinHelp (min oldMin v) f rest of
          Nothing ->
            if v <= oldMin then
              Just ( [], xs )
            else
              Nothing
          Just ( before, after ) ->
            Just ( x :: before, after )

cons : ( a, List a ) -> List a
cons ( x, xs ) =
  x :: xs
