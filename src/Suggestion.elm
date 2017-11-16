module Suggestion exposing (Suggestion, Msg(..), view, selection)

import Highlight exposing (Highlight)
import Substring exposing (Substring)

import Html exposing (Html, span, button, mark, text, textarea, pre)
import Html.Attributes exposing (style, attribute, class, id, readonly)
import Html.Events exposing
  (onMouseEnter, onMouseLeave, onFocus, onBlur, onClick)

type alias Suggestion =
  { s : String
  , fg : String
  , bg : String
  , firstRange : Substring
  , ranges : List Substring
  }

type Msg
  = Enter
  | Leave
  | Focus
  | Blur
  | Copied

view : Bool -> Suggestion -> Html Msg
view recentlyCopied suggestion =
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
                , suggestion.s
                , "\").select(); document.execCommand(\"Copy\");"
                ]
            )
        , onMouseEnter Enter
        , onMouseLeave Leave
        , onFocus Focus
        , onBlur Blur
        , onClick Copied
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
            , id suggestion.s
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
            [ text suggestion.s ]
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
            [ ( Highlight.view
                  ( Highlight
                      ""
                      suggestion.fg
                      suggestion.bg
                      (Substring 0 suggestion.s)
                  )
              )
            ]
        ]
    ]

selection : Suggestion -> ( Int, Int )
selection suggestion =
  ( suggestion.firstRange.i, Substring.stop suggestion.firstRange )
