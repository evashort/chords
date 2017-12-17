module Suggestion exposing (Suggestion, Msg(..), view, selection)

import Substring exposing (Substring)
import Swatch exposing (Swatch)

import Html exposing (Html, span, button, mark, text, textarea, pre)
import Html.Attributes exposing (style, attribute, class, id, readonly)
import Html.Events exposing
  (onMouseEnter, onMouseLeave, onFocus, onBlur, onClick)

type alias Suggestion =
  { replacement : String
  , swatchLists : ( List Swatch, List Swatch, List Swatch )
  , firstRange : Substring
  , ranges : List Substring
  }

type Msg
  = Enter ( String, Int )
  | Leave
  | Focus ( String, Int )
  | Blur
  | Copied ( String, Int )

view : Int -> Bool -> String -> Int -> Suggestion -> Html Msg
view key recentlyCopied source index suggestion =
  let
    swatches =
      let ( a, b, c ) = suggestion.swatchLists in
        case -key % 3 of
          0 -> a
          1 -> b
          _ -> c
  in let
    idString = source ++ toString index
  in
    span
      [ style
          [ ( "display", "inline-flex" )
          , ( "align-items", "stretch")
          , ( "position", "relative")
          , ( "margin-right", "3px")
          ]
      ]
      [ span
          [ style
              ( List.concat
                  [ [ ( "position", "absolute" )
                    , ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "border", "1px solid darkgray" )
                    , ( "border-radius", "3px" )
                    , ( "box-sizing", "border-box" )
                    , ( "background", "#e0e0e0" )
                    , ( "color", "gray" )
                    , ( "transition-property", "opacity" )
                    , ( "z-index", "1" )
                    , ( "display", "flex" )
                    , ( "align-items", "center")
                    , ( "padding", "1px 3px" )
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
          , onMouseEnter (Enter ( source, index ))
          , onMouseLeave Leave
          , onFocus (Focus ( source, index ))
          , onBlur Blur
          , onClick (Copied ( source, index ))
          , class "pressMe"
          , style
              [ ( "padding", "0px 3px" )
              , ( "border-width", "1px" )
              , ( "border-style", "solid" )
              , ( "border-radius", "3px 0px 0px 3px" )
              , ( "font", "inherit" )
              ]
          ]
          [ text "Copy" ]
      , span
          [ style
              [ ( "position", "relative" )
              , ( "font-family", "\"Lucida Console\", Monaco, monospace" )
              , ( "font-size", "13pt" )
              , ( "margin-left", "-1px" )
              ]
          ]
          [ textarea
              [ readonly True
              , id idString
              , style
                  [ ( "font", "inherit" )
                  , ( "width", "100%" )
                  , ( "height", "100%" )
                  , ( "padding", "3px" )
                  , ( "border", "1px solid darkgray")
                  , ( "border-left-color", "transparent" )
                  , ( "border-radius", "0px 3px 3px 0px")
                  , ( "margin", "0px" )
                  , ( "position", "absolute" )
                  , ( "resize", "none" )
                  , ( "overflow", "hidden" )
                  , ( "box-sizing", "border-box" )
                  , ( "background", "transparent" )
                  ]
              ]
              [ text suggestion.replacement ]
          , pre
              [ style
                  [ ( "font", "inherit" )
                  , ( "padding", "3px" )
                  , ( "border", "1px solid transparent")
                  , ( "margin", "0px" )
                  , ( "white-space", "nowrap" )
                  , ( "word-wrap", "break-word" )
                  , ( "color", "transparent" )
                  ]
              ]
              (List.map Swatch.view swatches)
          ]
      ]

selection : Suggestion -> ( Int, Int )
selection suggestion =
  ( suggestion.firstRange.i, Substring.stop suggestion.firstRange )
