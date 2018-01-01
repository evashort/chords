module Suggestion exposing
  (Suggestion, Msg(..), Lens, view, rangeSet, sort, groupByReplacement)

import Substring exposing (Substring)
import Swatch exposing (Swatch)

import Dict exposing (Dict)
import Html exposing (Html, span, button, mark, text, textarea, pre)
import Html.Attributes exposing (style, attribute, class, id, readonly, rows)
import Html.Events exposing
  (onMouseEnter, onMouseLeave, onFocus, onBlur, onClick)
import Set exposing (Set)

type alias Suggestion =
  { swatches : List Swatch
  , ranges : List Substring
  }

type Msg
  = AddLens Lens
  | RemoveLens Bool
  | Copied ( Int, String )

type alias Lens =
  { hover : Bool
  , index : Int
  }

view : Bool -> Int -> List Swatch -> Html Msg
view recentlyCopied index swatches =
  let
    idString = "suggestion" ++ toString index
  in let
    replacement = Swatch.concat swatches
  in
    span
      [ style
          [ ( "display", "inline-flex" )
          , ( "align-items", "stretch")
          , ( "position", "relative")
          , ( "margin-right", "3px")
          , ( "height", "26px")
          ]
      ]
      [ span
          [ style
              ( List.concat
                  [ [ ( "position", "absolute" )
                    , ( "width", "100%" )
                    , ( "border", "1px solid darkgray" )
                    , ( "border-radius", "3px" )
                    , ( "box-sizing", "border-box" )
                    , ( "background", "#e0e0e0" )
                    , ( "color", "gray" )
                    , ( "transition-property", "opacity" )
                    , ( "z-index", "1" )
                    , ( "padding", "0px 3px" )
                    , ( "line-height", "24px" )
                    , ( "pointer-events", "none" )
                    ]
                  , if recentlyCopied then
                      [ ( "transition-duration", "0s" )
                      , ( "opacity", "1" )
                      ]
                    else
                      [ ( "transition-duration", "0.5s" )
                      , ( "opacity", "0" )
                      ]
                  ]
              )
          ]
          [ text "Copied" ]
      , button
          [ attribute
              "onclick"
              ( String.concat
                  [ "document.getElementById(\""
                  , idString
                  , "\").select(); document.execCommand(\"Copy\");"
                  ]
              )
          , onMouseEnter (AddLens (Lens True index))
          , onMouseLeave (RemoveLens True)
          , onFocus (AddLens (Lens False index))
          , onBlur (RemoveLens False)
          , onClick (Copied ( index, replacement ))
          , class "pressMe"
          , style
              [ ( "padding", "0px 3px" )
              , ( "border-width", "1px" )
              , ( "border-style", "solid" )
              , ( "border-radius", "3px 0px 0px 3px" )
              , ( "font", "inherit" )
              , ( "line-height", "24px" )
              ]
          ]
          [ text "Copy" ]
      , span
          [ style
              [ ( "position", "relative" )
              , ( "font-family", "\"Lucida Console\", Monaco, monospace" )
              , ( "font-size", "13pt" )
              , ( "line-height", "18px" )
              ]
          ]
          [ textarea
              [ readonly True
              , id idString
              , rows 1
              , style
                  [ ( "font", "inherit" )
                  , ( "width", "100%" )
                  , ( "padding", "3px" )
                  , ( "border", "1px solid darkgray")
                  , ( "border-left", "none" )
                  , ( "border-radius", "0px 3px 3px 0px")
                  , ( "margin", "0px" )
                  , ( "position", "absolute" )
                  , ( "resize", "none" )
                  , ( "overflow", "hidden" )
                  , ( "box-sizing", "border-box" )
                  , ( "background", "transparent" )
                  ]
              ]
              [ text replacement ]
          , pre
              [ style
                  [ ( "font", "inherit" )
                  , ( "padding", "3px" )
                  , ( "border", "1px solid transparent")
                  , ( "border-left", "none")
                  , ( "margin", "0px" )
                  , ( "white-space", "nowrap" )
                  , ( "word-wrap", "break-word" )
                  , ( "color", "transparent" )
                  ]
              ]
              (List.map Swatch.view swatches)
          ]
      ]

rangeSet : Suggestion -> Set ( Int, Int )
rangeSet suggestion =
  Set.fromList (List.map Substring.range suggestion.ranges)

sort : List Suggestion -> List Suggestion
sort = List.sortBy (List.map .i << .ranges)

groupByReplacement :
  List ( List Swatch, Substring ) -> Dict String Suggestion
groupByReplacement suggestions =
  Dict.map (always reverseRanges) (groupByReplacementHelp suggestions)

reverseRanges : Suggestion -> Suggestion
reverseRanges suggestion =
  { suggestion | ranges = List.reverse suggestion.ranges }

groupByReplacementHelp :
  List ( List Swatch, Substring ) -> Dict String Suggestion
groupByReplacementHelp suggestions =
  case suggestions of
    [] -> Dict.empty
    ( swatches, range ) :: rest ->
      Dict.update
        (Swatch.concat swatches)
        (addRange swatches range)
        (groupByReplacementHelp rest)

addRange : List Swatch -> Substring -> Maybe Suggestion -> Maybe Suggestion
addRange swatches range suggestion =
  case suggestion of
    Nothing ->
      Just { swatches = swatches, ranges = [ range ] }
    Just suggestion ->
      Just { suggestion | ranges = range :: suggestion.ranges }
