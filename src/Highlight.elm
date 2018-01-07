module Highlight exposing (Highlight, mergeLayers)

import Swatch exposing (Swatch)
import Substring exposing (Substring)

type alias Highlight =
  { fg : String
  , bg : String
  , substring : Substring
  }

mergeLayers : List (List Highlight) -> List Swatch
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

mergeLayersHelp : Highlight -> Queue -> List Swatch
mergeLayersHelp highlight queue =
  case pop queue of
    Nothing ->
      [ { fg = highlight.fg
        , bg = highlight.bg
        , s = highlight.substring.s
        }
      ]
    Just ( highlightNew, queueNew ) ->
      { fg = highlight.fg
      , bg = highlight.bg
      , s =
          String.left
            (highlightNew.substring.i - highlight.substring.i)
            highlight.substring.s
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
