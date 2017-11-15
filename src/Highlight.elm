module Highlight exposing
  (Highlight, view, suggestDeletion, mergeLayers)

import Substring exposing (Substring)

import Html exposing (Html)
import Html.Attributes exposing (style)

type alias Highlight =
  { fg : String
  , bg : String
  , substring : Substring
  }

view : Highlight -> Html msg
view highlight =
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

suggestDeletion : Substring -> Highlight
suggestDeletion = Highlight "#ffffff" "#ff0000"

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
                  (highlight :: peers) ::
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
